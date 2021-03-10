-- manipulates the yaml data into the relevant data trees, evaluates from that
{-# LANGUAGE OverloadedStrings, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses #-}

module Evaluate
    ( outcomesToContextTree -- convert from a list of outcomes to a ContextTree
    , treeScoreFolder -- the function to use in foldTree when folding across scored outcomes
    , TreeMemoT, MemoTHash
    ) where

import Contexts
import ParseData
import Game
import Score
import ScoreData

import Data.List
import Data.Maybe
import Data.Function
import Data.Text (Text, unpack)
import Data.Tree
import Data.Hashable (Hashable)
import Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as HashMap
import Control.Monad.Memo (MemoStateT, MemoT, memo)
import qualified Control.Monad.Memo as Memo
import Control.Monad.Reader
import Control.Monad.State.Lazy

-- a Recontext with Maybe specified weights, because we need those!
data Outcome =
    Outcome { startContext::Context
            , result::Recontext
            , colWeight::(Maybe Double)
            , rowWeight::(Maybe Double)
    } deriving (Eq, Show)

-- a mixup with all the outcomes that require a different Context filtered out, and also the data tidied for our purposes
data MixupFiltered =
    MixupFiltered { mixupNameFiltered::Text
                  , outcomesFiltered::[Outcome]
    } deriving (Eq, Show)


-- take a Mixup, filter it or just give a context if it would be empty
mixupFilter :: Context -> Maybe Mixup -> Either (Text, Context) MixupFiltered
mixupFilter context (Just mix) = case and[evalState (compareAll (reqs mix)) context, evalState (compareNone (antireqs mix)) context] of
                                                                           True -> case outcomesFilter context (outcomes mix) (optionsFilter context . attOptions $ mix) (optionsFilter context . defOptions $ mix) of
                                                                                        [] -> Left (mname mix, context)
                                                                                        x -> Right . MixupFiltered (mname mix) . map (recontextToOutcome (attOptions mix) (defOptions mix)) $ x
                                                                           False -> Left (mname mix, context)
    where
        outcomesFilter :: Context -> [Recontext] -> HashMap Text Option -> HashMap Text Option -> [Recontext]
        outcomesFilter context outcomes atts defs = filter (\(Recontext att def _ _ _) -> (HashMap.member att atts) && (HashMap.member def defs)) outcomes
        
        optionsFilter :: Context -> HashMap Text Option -> HashMap Text Option
        optionsFilter c = HashMap.filter (\o -> and [evalState (compareAll (require o)) c, evalState (compareNone (antirequire o)) c])
        
        recontextToOutcome :: HashMap Text Option -> HashMap Text Option -> Recontext -> Outcome
        recontextToOutcome atts defs rec = Outcome context rec (optionWeight . optionFromRecontextCol atts $ rec) (optionWeight . optionFromRecontextRow defs $ rec)
        
        optionFromRecontextCol :: HashMap Text Option -> Recontext -> Option
        optionFromRecontextCol options rec = fromMaybe (error "missing attack option") . HashMap.lookup (colOption rec) $ options
        optionFromRecontextRow :: HashMap Text Option -> Recontext -> Option
        optionFromRecontextRow options rec = fromMaybe (error "missing defense option") . HashMap.lookup (rowOption rec) $ options
mixupFilter context Nothing = Left ("", context)

        

-- take the mixup data, the mixup name, the current context, and the list of outcomes - return a Tree describing the resulting structure recursively (for as long as there are nexts and the context is not an endstate), also carry through the MixupMetadata metadata for readability
outcomesToContextTree :: (Monad a) => ModuleReader a TreeContext
outcomesToContextTree = do
    mods <- askMods
    unfoldTreeM unfolder (Outcome newContext (outdatum mods) Nothing Nothing)
    where
        unfolder :: (Monad a) => Outcome -> ModuleReader a (TreeContextItem, [Outcome])
        unfolder o = do
            mods <- askMods
            (newcontext, mixmaybe, mnext) <- recontextMix o
            return ((mnext, (colOption . result $ o, colWeight o), (rowOption . result $ o, rowWeight o), newcontext), (either (const []) outcomesFiltered . mixupFilter newcontext $ mixmaybe))
        
        recontextMix :: (Monad a) => Outcome -> ModuleReader a (Context, Maybe Mixup, Maybe MixupMetadata)
        recontextMix o = do
            mods <- askMods
            newcontext <- updateContext . execState (addset (set . result $ o) (add . result $ o)) . startContext $ o
            ended <- endCheck newcontext
            let nextmix = if ended
                            then Nothing
                            else fmap (\next -> fromMaybe (errMix next) . HashMap.lookup next . mixdatum $ mods) . next . result $ o
            return (newcontext, nextmix, next . result $ o)
        
        errMix meta = error $ "No mixups with required attacker (" ++ (unpack . metaAtt $ meta) ++ ") and defender (" ++ (unpack . metaDef $ meta) ++ ") called '" ++ (unpack . metaName $ meta) ++ "' found."

instance (Ord k, Hashable k) => Memo.MapLike (HashMap k v) k v where
    add = HashMap.insert
    lookup = HashMap.lookup

type MemoTHash k v = MemoStateT (HashMap k v) k v
type TreeMemoT = MemoTHash [(Opt, Opt, Double, Double)] ResultSimple

treeScoreFolder :: (Monad a) => TreeContextItem -> [TreeMemoT (ModuleReader a) TreeGameItem] -> TreeMemoT (ModuleReader a) TreeGameItem
treeScoreFolder (meta,a,b,c) [] = do
    mods <- lift askMods
    scoresa <- lift $ scoresatt c
    scoresd <- lift $ scoresdef c
    let scorea = head scoresa
    let scored = head scoresd
    let g = [(a,b,scorea,scored)]
    res <- memo (pure . solveComplex) g
    return $ TreeGameItem c a b (Game "" (fromMaybe "" . fmap metaAtt $ meta) (fromMaybe "" . fmap metaDef $ meta) g . resultComplex g $ res) scoresa scoresd [] []
treeScoreFolder (meta,a,b,c) subgamesM = do
    outsC <- map (\tgi -> map (\x -> (tgiAtt tgi, tgiDef tgi, x)) $ tgiAttEVs tgi) <$> sequence subgamesM
    outsR <- map (\tgi -> map (\x -> (tgiAtt tgi, tgiDef tgi, x)) $ tgiDefEVs tgi) <$> sequence subgamesM
    
    evs <- map (\tgi -> (tgiAtt tgi, tgiDef tgi, evc . outcomesC . tgiGame $ tgi, evr . outcomesC . tgiGame $ tgi)) <$> sequence subgamesM
    res <- memo (pure . solveComplex) evs
    
    let gamesC = map (gameSimplePartial . gameSimplePartialOpts) . transpose $ outsC
        gamesR = map (gameSimplePartial . gameSimplePartialOpts) . transpose $ outsR
        
        wc = weightsColsSimple res
        wr = weightsRowsSimple res
        
        evsC = map (\g -> calcEVSimpleCore g wc wr) gamesC
        evsR = map (\g -> calcEVSimpleCore g wc wr) gamesR
        sdsC = map (\g -> calcSDSimpleCore g wc wr) gamesC
        sdsR = map (\g -> calcSDSimpleCore g wc wr) gamesR
    
    return $ TreeGameItem c a b (Game (fromMaybe "" . fmap metaName $ meta) (fromMaybe "" . fmap metaAtt $ meta) (fromMaybe "" . fmap metaDef $ meta) evs . resultComplex evs $ res) evsC evsR sdsC sdsR

scanTree f ~(Node r l) = Node r $ map (scan' r) l where
    scan' a ~(Node n b) = let a' = f a n in Node a' $ map (scan' r) b 

fst3 (x,_,_) = x
snd3 (_,x,_) = x
thd3 (_,_,x) = x
