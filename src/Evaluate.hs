-- manipulates the yaml data into the relevant data trees, evaluates from that
-- TODO: finish
{-# LANGUAGE OverloadedStrings #-}

module Evaluate
    ( test -- test scenario
    ) where

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
type TreeContext = Tree (Opt, Opt, Context)
type TreeScore = Tree (Opt, Opt, Double)

-- take a Mixup, filter it or just give a context if it would be empty
mixupFilter :: Context -> Maybe Mixup -> Either Context MixupFiltered
mixupFilter context (Just (Mixup mname mreq munreq atts defs outs)) = case compareToContext context mreq munreq of
                                                                           True -> case outcomesFilter context outs (filter (compareOptionToContext context) atts) (filter (compareOptionToContext context) defs) of
                                                                                        [] -> Left context
                                                                                        x -> Right . MixupFiltered mname . map (recontextToOutcome atts defs) $ x
                                                                           False -> Left context
    where
        outcomesFilter :: Context -> [Recontext] -> [Option] -> [Option] -> [Recontext]
        outcomesFilter context outcomes atts defs = filter (\(Recontext att def _ _ _) -> (att `elem` (map optionName atts)) && (def `elem` (map optionName defs))) outcomes

        compareOptionToContext :: Context -> Option -> Bool
        compareOptionToContext context (Option _ oreq ounreq _) = compareToContext context oreq ounreq

        compareToContext :: Context -> Context -> Context -> Bool
        compareToContext context reqs unreqs = (and . map (`elem` context) $ reqs) && (not . or . map (`elem` context) $ unreqs)
        
        recontextToOutcome :: [Option] -> [Option] -> Recontext -> Outcome
        recontextToOutcome atts defs rec = Outcome context rec (optionWeight . optionFromRecontextCol atts $ rec) (optionWeight . optionFromRecontextRow defs $ rec)
        
        optionFromRecontextCol :: [Option] -> Recontext -> Option
        optionFromRecontextCol options (Recontext att _ _ _ _) = maybe (error "missing attack option") id . find ((== att) . optionName) $ options
        optionFromRecontextRow :: [Option] -> Recontext -> Option
        optionFromRecontextRow options (Recontext _ def _ _ _) = maybe (error "missing defense option") id . find ((== def) . optionName) $ options
mixupFilter context Nothing = Left context



-- apply a Recontext and return a pair of the resulting Context and Maybe, if there was a next in the Recontext, the next Mixup
recontextMix :: [MixupGroup] -> Context -> Recontext -> (Context, Maybe Mixup)
recontextMix mgroups con r = (\newcon -> (newcon, if endCheck newcon then Nothing else mixupNext mgroups . next $ r)) $ recontext con r
    where
--         use a Maybe NextMixup to get a Maybe Mixup
        mixupNext :: [MixupGroup] -> Maybe NextMixup -> Maybe Mixup
        mixupNext mgroups (Just (NextMixup att def mix)) = Just . head . filter ((== mix) . mixupName) . mixups . head . filter ((== att) . attacker) . filter ((== def) . defender) $ mgroups
        mixupNext _ Nothing = Nothing
        
        -- for each context key, check if it's in set - if so, set it, else check if it's in add, and add it to that (or otherwise add it to 0)
        -- combine this with the sets not in context, and the adds in neither set nor context
        recontext :: Context -> Recontext -> Context
        recontext c r = contextCheck . foldr (\a b -> (recontextSingle r a):b) (recontextLeftovers c r) $ c
            where
                recontextSingle :: Recontext -> (Text, Integer) -> (Text, Integer)
                recontextSingle r (k,v) = (k, maybe ((+) v . maybe 0 id . lookup k $ add r) id . lookup k $ set r)
                
                recontextLeftovers :: Context -> Recontext -> Context
                recontextLeftovers c (Recontext _ _ s a _) = deleteFirstsBy ((==) `on` fst) (unionBy ((==) `on` fst) s a) c

-- take the mixup data, the mixup name, the current context, and the list of outcomes - return a Tree describing the resulting structure recursively (for as long as there are nexts and the context is not an endstate)
outcomesToContextTree :: [MixupGroup] -> Text -> Instruction -> TreeContext
outcomesToContextTree mgroup name (Instruction _ _ _ out) = unfoldTree unfolder (Outcome [] out Nothing Nothing)
    where
        unfolder :: Outcome -> (((Text, Maybe Double), (Text, Maybe Double), Context), [Outcome])
        unfolder o = do
            let (newcontext, mixmaybe) = recontextMix mgroup (startContext o) (result o)
            (((colOption . result $ o, colWeight o), (rowOption . result $ o, rowWeight o), newcontext), either (const []) outcomesFiltered . mixupFilter newcontext $ mixmaybe)

-- TODO: fold TreeScore such that the end result is an evaluated GameComplex
-- the fold function should first convert the summary values (of type (Opt, Opt, GameComplex)) to their EVs (Opt, Opt, Double), then turn those EVs into an (Opt, Opt, GameComplex {gameCName::Text, gameData::[((Text, Maybe Double), (Text, Maybe Double), Double)], outcomesC::(Maybe Result)})
-- treeScoreFolder :: (Opt, Opt, Context) -> [(Opt, Opt, GameComplex)] -> (Opt, Opt, GameComplex)

scanTree f ~(Node r l) = Node r $ map (scan' r) l where
    scan' a ~(Node n b) = let a' = f a n in Node a' $ map (scan' r) b 

test :: IO ()
test = do
    instructions <- readInstructions
    mgroups <- mapM instructionToMixupGroups instructions
    let contexttrees = map (\(mgroup,instr) -> outcomesToContextTree mgroup "Mix" instr) . zip mgroups $ instructions
    mapM_ (putStrLn . drawTree . fmap show) contexttrees
