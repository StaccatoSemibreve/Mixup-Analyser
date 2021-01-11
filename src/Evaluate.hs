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
outcomesToContextTree :: ModuleReader TreeContext
outcomesToContextTree = do
    mods <- ask
    unfoldTreeM unfolder (Outcome newContext (outdatum mods) Nothing Nothing)
    where
        unfolder :: Outcome -> ModuleReader (TreeContextItem, [Outcome])
        unfolder o = do
            mods <- ask
            (newcontext, mixmaybe, mnext) <- recontextMix o
            return ((mnext, (colOption . result $ o, colWeight o), (rowOption . result $ o, rowWeight o), newcontext), (either (const []) outcomesFiltered . mixupFilter newcontext $ mixmaybe))
        
        recontextMix :: Outcome -> ModuleReader (Context, Maybe Mixup, Maybe MixupMetadata)
        recontextMix o = do
            mods <- ask
            let newcontext = execState (updatum mods) . execState (addset (set . result $ o) (add . result $ o)) . startContext $ o
            let nextmix = if evalState (enddatum mods) newcontext
                            then Nothing
                            else fmap (\next -> fromMaybe (errMix next) . Map.lookup next . mixdatum $ mods) . next . result $ o
            return (newcontext, nextmix, next . result $ o)
        
        errMix meta = error $ "No mixups with required attacker (" ++ (unpack . metaAtt $ meta) ++ ") and defender (" ++ (unpack . metaDef $ meta) ++ ") called '" ++ (unpack . metaName $ meta) ++ "' found."

type TreeMemoT = MemoT [(Opt, Opt, Double, Double)] ResultSimple
treeScoreFolder :: TreeContextItem -> [TreeMemoT ModuleReader TreeGameItem] -> TreeMemoT ModuleReader TreeGameItem
treeScoreFolder (meta,a,b,c) [] = do
    mods <- lift ask
    let g = [(a,b,evalState (scoreattdatum mods) c, negate . evalState (scoredefdatum mods) $ c)]
    res <- memo (pure . solveComplexCore) g
    return (c,a,b, Game "" (fromMaybe "" . fmap metaAtt $ meta) (fromMaybe "" . fmap metaDef $ meta) g . Just . resultComplex g $ res)
treeScoreFolder (meta,a,b,c) subgamesM = do
    evs <- map (\(_, o1, o2, g) -> (o1, o2, evc . fromMaybe (error "???") . outcomesC $ g, evr . fromMaybe (error "???") . outcomesC $ g)) <$> sequence subgamesM
    res <- memo (pure . solveComplexCore) evs
    return (c,a,b, Game (fromMaybe "" . fmap metaName $ meta) (fromMaybe "" . fmap metaAtt $ meta) (fromMaybe "" . fmap metaDef $ meta) evs . Just . resultComplex evs $ res)

scanTree f ~(Node r l) = Node r $ map (scan' r) l where
    scan' a ~(Node n b) = let a' = f a n in Node a' $ map (scan' r) b 

fst3 (x,_,_) = x
snd3 (_,x,_) = x
thd3 (_,_,x) = x
