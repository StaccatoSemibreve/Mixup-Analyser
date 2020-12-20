-- manipulates the yaml data into the relevant data trees, evaluates from that
-- TODO: finish
{-# LANGUAGE OverloadedStrings #-}

module Evaluate
    ( outcomesToContextTree -- convert from a list of outcomes to a ContextTree
    , treeScoreFolder -- the function to use in foldTree when folding across scored outcomes
    ) where

import Contexts
import Parse
import Game
import Custom

import Data.List
import Data.Maybe
import Data.Function
import Data.Text (Text)
import Data.Tree

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
type TreeContext = Tree (Maybe NextMixup, Opt, Opt, Context)
type TreeScore = Tree (Opt, Opt, Double)

-- take a Mixup, filter it or just give a context if it would be empty
mixupFilter :: Context -> Maybe Mixup -> Either (Text, Context) MixupFiltered
mixupFilter context (Just (Mixup mname mreq munreq atts defs outs)) = case and[compareAll mreq context, compareNone munreq context] of
                                                                           True -> case outcomesFilter context outs (optionsFilter context atts) (optionsFilter context defs) of
                                                                                        [] -> Left (mname, context)
                                                                                        x -> Right . MixupFiltered mname . map (recontextToOutcome atts defs) $ x
                                                                           False -> Left (mname, context)
    where
        outcomesFilter :: Context -> [Recontext] -> [Option] -> [Option] -> [Recontext]
        outcomesFilter context outcomes atts defs = filter (\(Recontext att def _ _ _) -> (att `elem` (map optionName atts)) && (def `elem` (map optionName defs))) outcomes
        
        optionsFilter :: Context -> [Option] -> [Option]
        optionsFilter c = filter (\o -> and [compareAll (require o) c, compareNone (antirequire o) c])
        
        recontextToOutcome :: [Option] -> [Option] -> Recontext -> Outcome
        recontextToOutcome atts defs rec = Outcome context rec (optionWeight . optionFromRecontextCol atts $ rec) (optionWeight . optionFromRecontextRow defs $ rec)
        
        optionFromRecontextCol :: [Option] -> Recontext -> Option
        optionFromRecontextCol options (Recontext att _ _ _ _) = fromMaybe (error "missing attack option") . find ((== att) . optionName) $ options
        optionFromRecontextRow :: [Option] -> Recontext -> Option
        optionFromRecontextRow options (Recontext _ def _ _ _) = fromMaybe (error "missing defense option") . find ((== def) . optionName) $ options
mixupFilter context Nothing = Left ("", context)



-- apply a Recontext and return a pair of the resulting Context and Maybe, if there was a next in the Recontext, the next Mixup, also carry through the NextMixup metadata for readability
recontextMix :: [MixupGroup] -> Context -> Recontext -> (Context, Maybe Mixup, Maybe NextMixup)
recontextMix mgroups con r = (\newcon -> (newcon, if endCheck newcon then Nothing else mixupNext mgroups . next $ r, next r)) . contextCheck . addset (set r) (add r) $ con
    where
--         use a Maybe NextMixup to get a Maybe Mixup
        mixupNext :: [MixupGroup] -> Maybe NextMixup -> Maybe Mixup
        mixupNext mgroups (Just (NextMixup att def mix)) = Just . head . filter ((== mix) . mixupName) . mixups . head . filter ((== att) . attacker) . filter ((== def) . defender) $ mgroups
        mixupNext _ Nothing = Nothing

-- take the mixup data, the mixup name, the current context, and the list of outcomes - return a Tree describing the resulting structure recursively (for as long as there are nexts and the context is not an endstate), also carry through the NextMixup metadata for readability
outcomesToContextTree :: [MixupGroup] -> Instruction -> TreeContext
outcomesToContextTree mgroup (Instruction _ _ _ _ out) = unfoldTree unfolder (Outcome newContext out Nothing Nothing)
    where
        unfolder :: Outcome -> ((Maybe NextMixup, Opt, Opt, Context), [Outcome])
        unfolder o = do
            let (newcontext, mixmaybe, mnext) = recontextMix mgroup (startContext o) (result o)
            ((mnext, (colOption . result $ o, colWeight o), (rowOption . result $ o, rowWeight o), newcontext), either (const []) outcomesFiltered . mixupFilter newcontext $ mixmaybe)

-- the fold function should first convert the summary values (of type (Opt, Opt, GameComplex)) to their EVs (Opt, Opt, Double), then turn those EVs into an (Opt, Opt, GameComplex {gameCName::Text, gameData::[((Text, Maybe Double), (Text, Maybe Double), Double)], outcomesC::(Maybe Result)}), also carry through the NextMixup metadata for readability
treeScoreFolder :: (Context -> Double) -> (Maybe NextMixup, Opt, Opt, Context) -> [(Opt, Opt, GameComplex)] -> (Opt, Opt, GameComplex)
treeScoreFolder score (mnext,a,b,c) [] = (a,b, solveComplex $ gameComplex "" (fromMaybe "" . fmap nextAtt $ mnext) (fromMaybe "" . fmap nextDef $ mnext) [(a,b,score c)]) -- TODO: carry player names through
treeScoreFolder score (mnext,a,b,_) subgames = do
    let evs = map (\(o1, o2, g) -> (o1, o2, resCEV . fromMaybe (error "???") . outcomesC $ g)) subgames
    (a,b, solveComplex $ gameComplex (fromMaybe "" . fmap nextM $ mnext) (fromMaybe "" . fmap nextAtt $ mnext) (fromMaybe "" . fmap nextDef $ mnext) evs)
    

scanTree f ~(Node r l) = Node r $ map (scan' r) l where
    scan' a ~(Node n b) = let a' = f a n in Node a' $ map (scan' r) b 

fst3 (x,_,_) = x
snd3 (_,x,_) = x
thd3 (_,_,x) = x
