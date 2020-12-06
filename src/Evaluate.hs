-- manipulates the yaml data into the relevant data trees, evaluates from that
-- TODO: finish
{-# LANGUAGE OverloadedStrings #-}

module Evaluate
    ( followInstructions -- take mixup data, take instruction, return a context, Maybe with a Mixup if the instruction included a next (dunno why it wouldn't tbh, but it's easier to include a Maybe here anyway)
    , test -- test scenario
    ) where

import Parse
import Game
import Custom
import Data.List
import Data.Maybe
import Data.Function
import Data.Text (Text)

-- a Recontext with Maybe specified weights, because we need those!
data Outcome =
    Outcome { result::Recontext,
              attWeight::(Maybe Integer),
              defWeight::(Maybe Integer)
    } deriving (Eq, Show)

-- a mixup with all the outcomes that require a different Context filtered out, and also the data tidied for our purposes
data MixupFiltered =
    MixupFiltered { mixupNameFiltered::Text
                  , mixupContext::Context
                  , outcomesFiltered::[Outcome]
    } deriving (Eq, Show)

-- a tree-like structure with a name, a list of column options, a list of row options, then a 2d list of further trees
data MatrixTree a b = Node Text [a] [a] [[MatrixTree a b]] | Leaf b
    deriving (Eq, Show)
instance Functor (MatrixTree a) where
    fmap f (Node mname atts defs trees) = Node mname atts defs (map (map (fmap f)) trees)
    fmap f (Leaf value) = Leaf (f value)

-- a tree-like structure as above, except the lists are zipped into it (without duplication) TODO: SuperZippedMatrixTree, with duplication
data ZippedMatrixTree a b = ZipNode Text [(a, [(a, ZippedMatrixTree a b)])] | ZipLeaf b
    deriving (Eq, Show)
instance Functor (ZippedMatrixTree a) where
    fmap f (ZipNode mname trees) = ZipNode mname (map (fmap (map (fmap (fmap f)))) trees)
    fmap f (ZipLeaf value) = ZipLeaf (f value)

followInstructions :: [MixupGroup] -> Instruction -> (Context, Maybe Mixup)
followInstructions mgroups instr = recontextMix mgroups [] . context $ instr

-- take a Mixup, filter it or just give a context if it would be empty
mixupFilter :: Context -> Maybe Mixup -> Either Context MixupFiltered
mixupFilter context (Just (Mixup mname mreq munreq atts defs outs)) = case compareToContext context mreq munreq of
                                                                           True -> case outcomesFilter context outs (filter (compareOptionToContext context) atts) (filter (compareOptionToContext context) defs) of
                                                                                        [] -> Left context
                                                                                        x -> Right . MixupFiltered mname context . map (recontextToOutcome atts defs) $ x
                                                                           False -> Left context
    where
        outcomesFilter :: Context -> [Recontext] -> [Option] -> [Option] -> [Recontext]
        outcomesFilter context outcomes atts defs = filter (\(Recontext att def _ _ _) -> (att `elem` (map optionName atts)) && (def `elem` (map optionName defs))) outcomes

        compareOptionToContext :: Context -> Option -> Bool
        compareOptionToContext context (Option _ oreq ounreq _) = compareToContext context oreq ounreq

        compareToContext :: Context -> Context -> Context -> Bool
        compareToContext context reqs unreqs = (and . map (`elem` context) $ reqs) && (not . or . map (`elem` context) $ unreqs)
        
        recontextToOutcome :: [Option] -> [Option] -> Recontext -> Outcome
        recontextToOutcome atts defs rec = Outcome rec (optionWeight . optionFromRecontextAtt atts $ rec) (optionWeight . optionFromRecontextDef defs $ rec)
        
        optionFromRecontextAtt :: [Option] -> Recontext -> Option
        optionFromRecontextAtt options (Recontext att _ _ _ _) = maybe (error "missing attack option") id . find ((== att) . optionName) $ options
        optionFromRecontextDef :: [Option] -> Recontext -> Option
        optionFromRecontextDef options (Recontext _ def _ _ _) = maybe (error "missing defense option") id . find ((== def) . optionName) $ options
mixupFilter context Nothing = Left context

-- use a Maybe NextMixup to get a Maybe Mixup
mixupNext :: [MixupGroup] -> Maybe NextMixup -> Maybe Mixup
mixupNext mgroups (Just (NextMixup att def mix)) = Just . head . filter ((== mix) . mixupName) . mixups . head . filter ((== att) . attacker) . filter ((== def) . defender) $ mgroups
mixupNext _ Nothing = Nothing

-- apply a Recontext and return a pair of the resulting Context and Maybe, if there was a next in the Recontext, the next Mixup
recontextMix :: [MixupGroup] -> Context -> Recontext -> (Context, Maybe Mixup)
recontextMix mgroups con r = (\newcon -> (newcon, if endCheck newcon then Nothing else mixupNext mgroups . next $ r)) $ recontext con r

-- for each context key, check if it's in set - if so, set it, else check if it's in add, and add it to that (or otherwise add it to 0)
-- combine this with the sets not in context, and the adds in neither set nor context
recontext :: Context -> Recontext -> Context
recontext c r = contextCheck . foldr (\a b -> (recontextSingle r a):b) (recontextLeftovers c r) $ c
    where
        recontextSingle :: Recontext -> (Text, Integer) -> (Text, Integer)
        recontextSingle r (k,v) = (k, maybe ((+) v . maybe 0 id . lookup k $ add r) id . lookup k $ set r)
        
        recontextLeftovers :: Context -> Recontext -> Context
        recontextLeftovers c (Recontext _ _ s a _) = deleteFirstsBy ((==) `on` fst) (unionBy ((==) `on` fst) s a) c

-- evaluate :: [MixupGroup] -> Context -> Either Context MixupFiltered -> Double
-- evaluate mgroups con step = either score ((\(newcon, newmix) -> evaluate mgroups newcon $ mixupFilter newmix) . recontext mgroups con --oh no we need to do this to all subelements)

-- take the mixup data, the mixup name, the current context, and the list of outcomes - return a MatrixTree describing the resulting structure recursively (for as long as there are nexts and the context is not an endstate) TODO: rename
recontextsToPartialGame :: [MixupGroup] -> Text -> Context -> [Outcome] -> MatrixTree (Text, Maybe Integer) Context
recontextsToPartialGame mgroups name c r = do
    let atts = sortOn fst . nub . map (\out -> (attackerOption . result $ out, attWeight out)) $ r
    let defs = sortOn fst . nub . map (\out -> (defenderOption . result $ out, defWeight out)) $ r
    let conmix = map (map (recontextMix mgroups c . result)) . groupBy ((==) `on` (defenderOption . result)) . sortOn (defenderOption . result) . sortOn (attackerOption . result) $ r
    let filtmix = map (map (uncurry mixupFilter)) conmix
    Node name atts defs (map (map (either Leaf (\(MixupFiltered mname con outs) -> recontextsToPartialGame mgroups mname con outs))) filtmix)

-- does what it says on the tin really, dunno if i even need it, i just included it for completeness's sake - to be applied to any form of MatrixTree - score is from Custom! TODO: oh no i need to carry score functions through to here from the original Instruction, how the heck do i do that
scoreGame :: (Functor (a b)) => a b Context -> a b Double
scoreGame = fmap score

-- turn a normal matrix tree into a zipped one, or vice versa for unzip
zipTree :: MatrixTree a b -> ZippedMatrixTree a b
zipTree (Leaf l) = ZipLeaf l
zipTree (Node treename cols rows subtrees) = ZipNode treename (zip rows (map (zip cols . fmap zipTree) subtrees))

unzipTree :: ZippedMatrixTree a b -> MatrixTree a b
unzipTree (ZipLeaf l) = Leaf l
unzipTree (ZipNode treename subtrees) = Node treename (fst . head . map (unzip . snd) $ subtrees) (fst . unzip $ subtrees) (map (map (unzipTree . snd) . snd) subtrees)

-- return a filtered tree such that only options without specified weights are included, for use in evaluation using Game TODO: i need to handle cases where exactly one weight is specified, for which i should use a SuperZippedMatrixTree that i haven't put together yet
filterGame :: ZippedMatrixTree (Text, Maybe Integer) a -> ZippedMatrixTree (Text, Maybe Integer) a
filterGame (ZipLeaf val) = ZipLeaf val
filterGame (ZipNode mname subtrees) = ZipNode mname (map (fmap (filter (isNothing . snd . fst))) . filter (isNothing . snd . fst) $ subtrees)

-- make sure it all works, using what is very similar (perhaps identical) to the final code that will be in Main tbh, but it's easier to work with here for now anyway
test :: IO ()
test = do
    instructions <- readInstructions
    mgroups <- mapM instructionToMixupGroups instructions
    let starts = zipWith followInstructions mgroups instructions
    mapM_ (\(c,m) -> putStrLn . show . unzipTree . fmap score . filterGame . zipTree . recontextsToPartialGame (head mgroups) "Mix" c . either (\_ -> []) outcomesFiltered . uncurry mixupFilter $ (c,m)) starts
