-- manipulates the yaml data into the relevant data trees, evaluates from that
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Evaluate
    ( outcomesToContextTree -- convert from a list of outcomes to a ContextTree
    , treeScoreFolder -- the function to use in foldTree when folding across scored outcomes
    , TreeMemoT
    ) where

import Contexts
import ParseData
import Game
import GameSolve
import Score

import Data.List
import Data.Maybe
import Data.Function
import Data.Text (Text, unpack)
import Data.Tree
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad.Memo (MemoT, memo)
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
        outcomesFilter :: Context -> [Recontext] -> Map Text Option -> Map Text Option -> [Recontext]
        outcomesFilter context outcomes atts defs = filter (\(Recontext att def _ _ _) -> (Map.member att atts) && (Map.member def defs)) outcomes
        
        optionsFilter :: Context -> Map Text Option -> Map Text Option
        optionsFilter c = Map.filter (\o -> and [evalState (compareAll (require o)) c, evalState (compareNone (antirequire o)) c])
        
        recontextToOutcome :: Map Text Option -> Map Text Option -> Recontext -> Outcome
        recontextToOutcome atts defs rec = Outcome context rec (optionWeight . optionFromRecontextCol atts $ rec) (optionWeight . optionFromRecontextRow defs $ rec)
        
        optionFromRecontextCol :: Map Text Option -> Recontext -> Option
        optionFromRecontextCol options rec = fromMaybe (error "missing attack option") . Map.lookup (colOption rec) $ options
        optionFromRecontextRow :: Map Text Option -> Recontext -> Option
        optionFromRecontextRow options rec = fromMaybe (error "missing defense option") . Map.lookup (rowOption rec) $ options
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
                            else fmap (\next -> fromMaybe (errMix next) . Map.lookup next . mixdatum $ mods) . next . result $ o
            return (newcontext, nextmix, next . result $ o)
        
        errMix meta = error $ "No mixups with required attacker (" ++ (unpack . metaAtt $ meta) ++ ") and defender (" ++ (unpack . metaDef $ meta) ++ ") called '" ++ (unpack . metaName $ meta) ++ "' found."

type TreeMemoT = MemoT [(Opt, Opt, Double, Double)] ResultSimple
treeScoreFolder :: (Monad a) => TreeContextItem -> [TreeMemoT (ModuleReader a) TreeGameItem] -> TreeMemoT (ModuleReader a) TreeGameItem
treeScoreFolder (meta,a,b,c) [] = do
    mods <- lift askMods
    scoresa <- lift $ scoresatt c
    scoresd <- lift $ scoresdef c
    let scorea = head scoresa
    let scored = head scoresd
    let g = [(a,b,scorea,scored)]
    res <- memo (pure . solveComplexCore) g
    return $ TreeGameItem c a b (Game "" (fromMaybe "" . fmap metaAtt $ meta) (fromMaybe "" . fmap metaDef $ meta) g . Just . resultComplex g $ res) scoresa scoresd [] []
treeScoreFolder (meta,a,b,c) subgamesM = do
    outsC <- map (\tgi -> map (\x -> (tgiAtt tgi, tgiDef tgi, x)) $ tgiAttEVs tgi) <$> sequence subgamesM
    outsR <- map (\tgi -> map (\x -> (tgiAtt tgi, tgiDef tgi, x)) $ tgiDefEVs tgi) <$> sequence subgamesM
    
    evs <- map (\tgi -> (tgiAtt tgi, tgiDef tgi, evc . fromMaybe (error "???") . outcomesC . tgiGame $ tgi, evr . fromMaybe (error "???") . outcomesC . tgiGame $ tgi)) <$> sequence subgamesM
    res <- memo (pure . solveComplexCore) evs
    
    let gamesC = map (gameSimplePartial . gameSimplePartialOpts) . transpose $ outsC
        gamesR = map (gameSimplePartial . gameSimplePartialOpts) . transpose $ outsR
        
        wc = weightsColsSimple res
        wr = weightsRowsSimple res
        
        evsC = map (\g -> calcEVSimpleCore g wc wr) gamesC
        evsR = map (\g -> calcEVSimpleCore g wc wr) gamesR
        sdsC = map (\g -> calcSDSimpleCore g wc wr) gamesC
        sdsR = map (\g -> calcSDSimpleCore g wc wr) gamesR
    
    return $ TreeGameItem c a b (Game (fromMaybe "" . fmap metaName $ meta) (fromMaybe "" . fmap metaAtt $ meta) (fromMaybe "" . fmap metaDef $ meta) evs . Just . resultComplex evs $ res) evsC evsR sdsC sdsR

scanTree f ~(Node r l) = Node r $ map (scan' r) l where
    scan' a ~(Node n b) = let a' = f a n in Node a' $ map (scan' r) b 

fst3 (x,_,_) = x
snd3 (_,x,_) = x
thd3 (_,_,x) = x
