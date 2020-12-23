-- manipulates the yaml data into the relevant data trees, evaluates from that
-- TODO: memoisation
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Evaluate
    ( outcomesToContextTree -- convert from a list of outcomes to a ContextTree
    , treeScoreFolder -- the function to use in foldTree when folding across scored outcomes
    , treeScoreFolderM
    , TreeContext
    , TreeGame
    ) where

import Contexts
import Parse
import Game

import Data.List
import Data.Maybe
import Data.Function
import Data.Text (Text, unpack)
import Data.Tree
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad.Memo (MonadMemo, memo)
import qualified Control.Monad.Memo as Memo


type Score = Context -> Double
type EndState = Context -> Bool
type Updater = Context -> Context
type Printer = TreeGame -> Text

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

type Opt = (Text, Maybe Double)
type TreeContextItem = (Maybe NextMixup, Opt, Opt, Context)
type TreeContext = Tree TreeContextItem
type TreeGameItem = (Context, Opt, Opt, GameComplex)
type TreeGame = Tree TreeGameItem
type TreeScore = Tree (Opt, Opt, Double)

-- take a Mixup, filter it or just give a context if it would be empty
mixupFilter :: Context -> Maybe Mixup -> Either (Text, Context) MixupFiltered
mixupFilter context (Just (Mixup mname mreq munreq atts defs outs)) = case and[compareAll mreq context, compareNone munreq context] of
                                                                           True -> case outcomesFilter context outs (optionsFilter context atts) (optionsFilter context defs) of
                                                                                        [] -> Left (mname, context)
                                                                                        x -> Right . MixupFiltered mname . map (recontextToOutcome atts defs) $ x
                                                                           False -> Left (mname, context)
    where
        outcomesFilter :: Context -> [Recontext] -> Map Text Option -> Map Text Option -> [Recontext]
        outcomesFilter context outcomes atts defs = filter (\(Recontext att def _ _ _) -> (Map.member att atts) && (Map.member def defs)) outcomes
        
        optionsFilter :: Context -> Map Text Option -> Map Text Option
        optionsFilter c = Map.filter (\o -> and [compareAll (require o) c, compareNone (antirequire o) c])
        
        recontextToOutcome :: Map Text Option -> Map Text Option -> Recontext -> Outcome
        recontextToOutcome atts defs rec = Outcome context rec (optionWeight . optionFromRecontextCol atts $ rec) (optionWeight . optionFromRecontextRow defs $ rec)
        
        optionFromRecontextCol :: Map Text Option -> Recontext -> Option
        optionFromRecontextCol options rec = fromMaybe (error "missing attack option") . Map.lookup (colOption rec) $ options
        optionFromRecontextRow :: Map Text Option -> Recontext -> Option
        optionFromRecontextRow options rec = fromMaybe (error "missing defense option") . Map.lookup (rowOption rec) $ options
mixupFilter context Nothing = Left ("", context)



-- apply a Recontext and return a pair of the resulting Context and Maybe, if there was a next in the Recontext, the next Mixup, also carry through the NextMixup metadata for readability
recontextMix :: MixupData -> (Context -> Bool) -> (Context -> Context) -> Context -> Recontext -> (Context, Maybe Mixup, Maybe NextMixup)
recontextMix mgroups endCheck contextUpdate con r = (\newcon -> (newcon, if endCheck newcon then Nothing else mixupNext mgroups . next $ r, next r)) . contextUpdate . addset (set r) (add r) $ con
    where
--         use a Maybe NextMixup to get a Maybe Mixup
        mixupNext :: MixupData -> Maybe NextMixup -> Maybe Mixup
        mixupNext mgroups nextMaybe = fmap (\next -> fromMaybe (errMix next) . Map.lookup next $ mgroups) nextMaybe
        
        errMix (NextMixup att def mix) = error $ "No mixups with required attacker (" ++ (unpack att) ++ ") and defender (" ++ (unpack def) ++ ") called '" ++ (unpack mix) ++ "' found."

-- take the mixup data, the mixup name, the current context, and the list of outcomes - return a Tree describing the resulting structure recursively (for as long as there are nexts and the context is not an endstate), also carry through the NextMixup metadata for readability
outcomesToContextTree :: MixupData -> (Context -> Bool) -> (Context -> Context) -> Instruction -> TreeContext
outcomesToContextTree mgroup f1 f2 (Instruction _ _ _ out) = unfoldTree unfolder (Outcome newContext out Nothing Nothing)
    where
--         unfolderm :: (MonadMemo Outcome (TreeContextItem, [Outcome]) m) => Outcome -> m (TreeContextItem, [Outcome])
--         unfolderm o = do
--             let (newcontext, mixmaybe, mnext) = recontextMix mgroup f1 f2 (startContext o) (result o)
--             return ((mnext, (colOption . result $ o, colWeight o), (rowOption . result $ o, rowWeight o), newcontext), either (const []) outcomesFiltered . mixupFilter newcontext $ mixmaybe)
            
        
        unfolder :: Outcome -> (TreeContextItem, [Outcome])
        unfolder o = do
            let (newcontext, mixmaybe, mnext) = recontextMix mgroup f1 f2 (startContext o) (result o)
            ((mnext, (colOption . result $ o, colWeight o), (rowOption . result $ o, rowWeight o), newcontext), either (const []) outcomesFiltered . mixupFilter newcontext $ mixmaybe)

-- the fold function should first convert the summary values (of type (Opt, Opt, GameComplex)) to their EVs (Opt, Opt, Double), then turn those EVs into an (Opt, Opt, GameComplex {gameCName::Text, gameData::[((Text, Maybe Double), (Text, Maybe Double), Double)], outcomesC::(Maybe Result)}), also carry through the NextMixup metadata for readability
treeScoreFolder :: Score -> TreeContextItem -> [TreeGameItem] -> TreeGameItem
treeScoreFolder score (mnext,a,b,c) [] = (c,a,b, solveComplex $ gameComplex "" (fromMaybe "" . fmap nextAtt $ mnext) (fromMaybe "" . fmap nextDef $ mnext) [(a,b,score c)])
treeScoreFolder score (mnext,a,b,c) subgames = let
                                                   evs = map (\(_, o1, o2, g) -> (o1, o2, resCEV . fromMaybe (error "???") . outcomesC $ g)) subgames
                                               in
                                                   (c,a,b, solveComplex $ gameComplex (fromMaybe "" . fmap nextM $ mnext) (fromMaybe "" . fmap nextAtt $ mnext) (fromMaybe "" . fmap nextDef $ mnext) evs)

treeScoreFolderM :: (MonadMemo GameComplex GameComplex m) => Score -> TreeContextItem -> [m TreeGameItem] -> m TreeGameItem
treeScoreFolderM score (mnext,a,b,c) [] = do
    let cpx = gameComplex "" (fromMaybe "" . fmap nextAtt $ mnext) (fromMaybe "" . fmap nextDef $ mnext) [(a,b,score c)]
    res <- memo (pure . solveComplex) cpx
    return (c,a,b,res)
treeScoreFolderM score (mnext,a,b,c) subgamesM = do
    evs <- map (\(_, o1, o2, g) -> (o1, o2, resCEV . fromMaybe (error "???") . outcomesC $ g)) <$> sequence subgamesM
    let cpx = gameComplex (fromMaybe "" . fmap nextM $ mnext) (fromMaybe "" . fmap nextAtt $ mnext) (fromMaybe "" . fmap nextDef $ mnext) evs
    res <- memo (pure . solveComplex) cpx
    return (c,a,b, res)

scanTree f ~(Node r l) = Node r $ map (scan' r) l where
    scan' a ~(Node n b) = let a' = f a n in Node a' $ map (scan' r) b 

fst3 (x,_,_) = x
snd3 (_,x,_) = x
thd3 (_,_,x) = x
