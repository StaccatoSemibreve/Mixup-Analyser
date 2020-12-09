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
              attWeight::(Maybe Double),
              defWeight::(Maybe Double)
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

-- a tree-like structure as above, except the lists are zipped into it (without duplication)
data ZippedMatrixTree a b = ZipNode Text [(a, [(a, ZippedMatrixTree a b)])] | ZipLeaf b
    deriving (Eq, Show)
instance Functor (ZippedMatrixTree a) where
    fmap f (ZipNode mname trees) = ZipNode mname (map (fmap (map (fmap (fmap f)))) trees)
    fmap f (ZipLeaf value) = ZipLeaf (f value)

-- turn a normal matrix tree into a zipped one, or vice versa for unzip
zipTree :: MatrixTree a b -> ZippedMatrixTree a b
zipTree (Leaf l) = ZipLeaf l
zipTree (Node treename cols rows subtrees) = ZipNode treename (zip rows (map (zip cols . fmap zipTree) subtrees))

unzipTree :: ZippedMatrixTree a b -> MatrixTree a b
unzipTree (ZipLeaf l) = Leaf l
unzipTree (ZipNode treename subtrees) = Node treename (fst . head . map (unzip . snd) $ subtrees) (fst . unzip $ subtrees) (map (map (unzipTree . snd) . snd) subtrees)

data SuperZippedMatrixTree a b = SuperZipNode Text [[(a, a, SuperZippedMatrixTree a b)]] | SuperZipLeaf b
    deriving (Eq, Show)
instance Functor (SuperZippedMatrixTree a) where
    fmap f (SuperZipNode mname trees) = SuperZipNode mname $ map (map (\(x,y,z) -> (x,y, fmap f z))) trees
    fmap f (SuperZipLeaf value) = SuperZipLeaf (f value)

-- turn a zipped matrix tree into a superzipped one, or vice versa for unzip
superzipTree :: ZippedMatrixTree a b -> SuperZippedMatrixTree a b
superzipTree (ZipLeaf l) = SuperZipLeaf l
superzipTree (ZipNode treename subtrees) = SuperZipNode treename $ map (\(a, b) -> map (\(c,d) -> (a,c, superzipTree d)) $ b) subtrees

-- unsuperzipTree :: SuperZippedMatrixTree a b -> ZippedMatrixTree a b
unsuperzipTree (SuperZipLeaf l) = ZipLeaf l
unsuperzipTree (SuperZipNode treename subtrees) = do
    let rows = map (head . map fst3) subtrees
    let cols = head . map (map snd3) $ subtrees
    let outs = map (map thd3) $ subtrees
    
    ZipNode treename (zip rows (map (zip cols . fmap unsuperzipTree) outs))
    where
        fst3 (x,_,_) = x
        snd3 (_,x,_) = x
        thd3 (_,_,x) = x

-- unsuperzipTree :: ZippedMatrixTree a b -> MatrixTree a b
-- unsuperzipTree (ZipLeaf l) = Leaf l
-- unsuperzipTree (ZipNode treename subtrees) = Node treename (fst . head . map (unzip . snd) $ subtrees) (fst . unzip $ subtrees) (map (map (unzipTree . snd) . snd) subtrees)

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

-- take the mixup data, the mixup name, the current context, and the list of outcomes - return a MatrixTree describing the resulting structure recursively (for as long as there are nexts and the context is not an endstate)
outcomesToContextTree :: [MixupGroup] -> Text -> Context -> [Outcome] -> MatrixTree (Text, Maybe Double) Context
outcomesToContextTree mgroups name c r = do
    let atts = sortOn fst . nub . map (\out -> (attackerOption . result $ out, attWeight out)) $ r
    let defs = sortOn fst . nub . map (\out -> (defenderOption . result $ out, defWeight out)) $ r
    let conmix = map (map (recontextMix mgroups c . result)) . groupBy ((==) `on` (defenderOption . result)) . sortOn (defenderOption . result) . sortOn (attackerOption . result) $ r
    let filtmix = map (map (uncurry mixupFilter)) conmix
    Node name atts defs (map (map (either Leaf (\(MixupFiltered mname con outs) -> outcomesToContextTree mgroups mname con outs))) filtmix)

-- does what it says on the tin really, dunno if i even need it, i just included it for completeness's sake - to be applied to any form of MatrixTree - score is from Custom! TODO: oh no i need to carry score functions through to here from the original Instruction, how the heck do i do that
scoreGame :: (Functor (a b)) => a b Context -> a b Double
scoreGame = fmap score

-- functionality: each fixed element is a new submixup, take the weighted mean of the opponent's option weights
-- evalGame :: SuperZippedMatrixTree (Text, Maybe Integer) a -> [SuperZippedMatrixTree (Text, Maybe Integer) a]
-- evalGame (SuperZipLeaf val) = [SuperZipLeaf val]
-- aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
evalGame (SuperZipNode mname subtrees) = do
--      !!0: no fixed weights, !!1: fixed rows, !!2: fixed cols, !!3: both fixed
    let fixedness = zip ["unfixed", "rowfixed", "colfixed", "allfixed"] [SuperZipNode mname (map (filter (\x -> and[isNothing . snd . fst3 $ x, isNothing . snd . snd3 $ x])) subtrees), SuperZipNode mname (map (filter (\x -> and[not . isNothing . snd . fst3 $ x, isNothing . snd . snd3 $ x])) subtrees), SuperZipNode mname (map (filter (\x -> and[isNothing . snd . fst3 $ x, not . isNothing . snd . snd3 $ x])) subtrees), SuperZipNode mname (map (filter (\x -> and[not . isNothing . snd . fst3 $ x, not . isNothing . snd . snd3 $ x])) subtrees)]
    let rowweights = sum . map (maybe 0 id . snd) . map fst3 . head $ subtrees
    let colweights = sum . map (maybe 0 id . snd) . map snd3 . head $ subtrees
--     let fixednessMapped = map eval fixedness
    (rowweights, colweights, fixedness!!0)
    where
        fst3 (x,_,_) = x
        snd3 (_,x,_) = x
        thd3 (_,_,x) = x
        
--         toGame :: SuperZippedMatrixTree (Text, Maybe Double) Double -> Game
--         toGame (SuperZipNode gamename trees) = do
--             let rows = map (head . map fst3) subtrees
--             let cols = head . map (map snd3) $ subtrees
--             let outs = map (map (quickndirty . thd3)) $ subtrees
--             game gamename cols rows outs
--             
        quickndirty :: SuperZippedMatrixTree (Text, Maybe Double) Double -> Double
        quickndirty (SuperZipLeaf val) = val
        quickndirty _ = 0
        
--         eval :: (String, SuperZippedMatrixTree) => Double
--         eval "unfixed" (SuperZipNode mname subtrees) = evalGame 

-- make sure it all works, using what is very similar (perhaps identical) to the final code that will be in Main tbh, but it's easier to work with here for now anyway
test :: IO ()
test = do
    instructions <- readInstructions
    mgroups <- mapM instructionToMixupGroups instructions
    let starts = zipWith followInstructions mgroups instructions
    let contexttrees = map (\(c,m) -> outcomesToContextTree (head mgroups) "Mix" c . either (const []) outcomesFiltered . mixupFilter c $ m) starts
    mapM_ (putStrLn . show . evalGame . superzipTree . zipTree . fmap score) contexttrees
