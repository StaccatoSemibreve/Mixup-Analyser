-- calculates nash equilibriums, does related backend stuff
{-# LANGUAGE OverloadedStrings #-}

module Game
    ( gameSimplePartial, gameSimplePartialOpts, gameSimple
    , calcPureEVs, calcEVSimpleCore, calcVarSimpleCore, calcSDSimpleCore
    , gameComplex, resultComplex
    , solvePure, solvePureBy, solvePureOn, solvePureZeroSum
    , solvePureOptimal, solvePureOptimalZeroSum
    , solveSimpleCoreNaive
    , calcEVSimpleCoreNaive
    , solveSimple, solveComplex
    , resultComplex
    , Game (Game, gameCName, attCName, defCName, gameData, outcomesC)
    , Result (Result, evc, evr, sdc, sdr, weightsAtts, weightsDefs)
    , ResultSimple  (ResultSimple, evSimpleCol, evSimpleRow, sdSimpleCol, sdSimpleRow, weightsColsSimple, weightsRowsSimple)
    ) where

import GameData

import Data.List
import Data.List.Split
import Data.Function
import Data.Maybe
import Data.Text (Text, unpack)
import Numeric.LinearAlgebra
import Numeric.LinearAlgebra.Data
import Data.Map (Map)
import qualified Data.Map as Map
import Safe

type EV = Double
type Var = Double
type SD = Double
type Weights = [Double]
type Coord = (Int,Int)
type RemovedElements = [Int]
type WeightedOption = (Opt, Opt, Double)
type BiWeightedOption = (Opt, Opt, Double, Double)
type PayoffsRaw = [[Double]]
type Payoffs = Matrix Double

gameSimplePartial :: PayoffsRaw -> Payoffs
gameSimplePartial = fromLists

gameSimplePartialOpts :: [WeightedOption] -> PayoffsRaw
gameSimplePartialOpts opts =
    let
        fst3 (x,_,_) = x
        snd3 (_,x,_) = x
        thd3 (_,_,x) = x
        
        cols = nub . map (fst . fst3) $ opts
        rows = nub . map (fst . snd3) $ opts
        grid = map (map thd3) . transpose . chunksOf (length rows) $ opts
    in
        grid

gameSimple :: Text -> [Text] -> [Text] -> PayoffsRaw -> PayoffsRaw -> GameSimple
gameSimple n c r m1 m2 = GameSimple n c r (fromLists m1) (fromLists m2) $ solveSimple m1 m2

calcPureEVs :: Payoffs -> Weights -> [EV]
calcPureEVs m cols = concat . toLists $ m Numeric.LinearAlgebra.<> (col (fmap (/ sum cols) cols))

calcEVSimpleCore :: Payoffs -> Weights -> Weights -> EV
calcEVSimpleCore m cols rows = (head . head . toLists $ (row rows) Numeric.LinearAlgebra.<> m Numeric.LinearAlgebra.<> (col cols))

calcVarSimpleCore :: Payoffs -> Weights -> Weights -> Var
calcVarSimpleCore mat cols rows = calcEVSimpleCore (cmap (\x -> (x - calcEVSimpleCore mat cols rows)**2) mat) cols rows

calcSDSimpleCore :: Payoffs -> Weights -> Weights -> SD
calcSDSimpleCore a b = sqrt . calcVarSimpleCore a b

gameComplex :: Text -> Text -> Text -> [BiWeightedOption] -> Game
gameComplex t1 t2 t3 m = Game t1 t2 t3 (sort m) . resultComplex m $ solveComplex m

resultComplex :: [(Opt, Opt, Double, Double)] -> ResultSimple -> Result
resultComplex opts (ResultSimple evc evr sdc sdr cw rw) = do
    let colnames = nub . map (fst . fst4) $ opts
    let rownames = nub . map (fst . snd4) $ opts
    Result evc evr sdc sdr (zip colnames cw) (zip rownames rw)


-- from here on out, stuff for solving games



-- the fundamental algorithm, given some ordering method to know which values are better
solvePureBy :: (Eq a) => (a -> a -> Ordering) -> [[a]] -> [[a]] -> [Coord]
solvePureBy f mc mr =
    let
        width = length . head $ mc
        height = length mc
        
        -- column player: picks the highest columns for each row
        mcIsBestResponse = map (\col -> map (== maximumBy f col) col) mc
        -- row player: picks the highest rows for each coloumn
        mrIsBestResponse = transpose . map (\row -> map (== maximumBy f row) row) . transpose $ mr
        bestResponses = map (uncurry zip) $ zip mcIsBestResponse mrIsBestResponse
        
        coords = map (\r -> map (\c -> (c,r)) [0..width-1]) [0..height-1]
        zipped = map (uncurry zip) $ zip coords bestResponses
        zippedFiltered = map fst . filter (\((x,y),(c,r)) -> and[c,r]) . concat $ zipped
    in
        zippedFiltered

-- solvePureBy applied to mapped values
solvePureOn :: (Ord c) => (a -> c) -> (b -> c) -> [[a]] -> [[b]] -> [Coord]
solvePureOn f g mc mr = solvePureBy compare (map (map f) mc) (map (map g) mr)

solvePure :: (Ord a) => [[a]] -> [[a]] -> [Coord]
solvePure mc mr = solvePureOn id id mc mr

solvePureZeroSum :: (Ord a, Num a) => [[a]] -> [Coord]
solvePureZeroSum m = solvePureOn id negate m m

evPure :: [[a]] -> Coord -> a
evPure m (c,r) = (m!!c)!!r

solvePureOptimalBy :: (Ord a) => (a -> a -> Ordering) -> ([[a]] -> Coord -> a) -> [[a]] -> [[a]] -> Maybe Coord
solvePureOptimalBy f ev mc mr =
    let
        pures = solvePureBy f mc mr
        -- note that we have already optimised the row choice per column choice, as these are all, by definition, nash equilibria
        -- thus, simply choose the optimal nash equilibrium for column ev
        bestPure = maximumByMay (compare `on` (\p -> ev mc p)) pures
    in
        bestPure

solvePureOptimalOn :: (Ord c) => (a -> c) -> (b -> c) -> ([[c]] -> Coord -> c) -> [[a]] -> [[b]] -> Maybe Coord
solvePureOptimalOn f g ev mc mr = solvePureOptimalBy compare ev (map (map f) mc) (map (map g) mr)

solvePureOptimal :: (Ord a) => [[a]] -> [[a]] -> Maybe Coord
solvePureOptimal mc mr = solvePureOptimalOn id id evPure mc mr

solvePureOptimalZeroSum :: (Ord a, Num a) => [[a]] -> Maybe Coord
solvePureOptimalZeroSum m = solvePureOptimal m . map (map negate) $ m


solveSimpleCoreNaive :: Matrix Double -> Maybe Weights
solveSimpleCoreNaive m =
    do
        case (rows m, cols m) of
            -- not really much choice
            (_,1) -> return [1]
            -- they only have one option, so zip your options to their position, get the best option's position, and pad a [1] with zeroes as appropriate to the position
            (1,c) -> return . (\n -> ((take n (repeat 0)) ++ [1] ++ (take (c - n - 1) (repeat 0)))) . fst . maximumBy (compare `on` snd) . zip [0..] . concat . toLists $ m
            -- use linear algebra to find the weights such that unexploitable play is achieved, ie, the ev of each of the opponent's options is equal
            _ -> let
                        weights = normalise . concat . toLists $ ((pinv m) Numeric.LinearAlgebra.<> (((rows m)><1) (repeat (fromInteger 1))))
                    in
                        if all (>=0) weights
                        then return weights
                        else Nothing
    where
        normalise :: (Fractional a) => [a] -> [a]
        normalise xs = map (/ (sum xs)) xs

calcEVSimpleCoreNaive :: Matrix Double -> Maybe EV
calcEVSimpleCoreNaive m = 
    do
        wc <- solveSimpleCoreNaive m
        wr <- solveSimpleCoreNaive . tr' $ m
        return $ calcEVSimpleCore m wc wr

gameSupports :: (Ord a, Num a) => [[a]] -> [[a]] -> [[((RemovedElements, RemovedElements), ([[a]], [[a]]))]]
gameSupports m1 m2 =
    let
        widther = length . head
        heighter = length
        w = widther m1 - 1
        h = heighter m1 - 1
        
        removeIndexes :: RemovedElements -> [a] -> [a]
        removeIndexes [] xs = xs
        removeIndexes _ [] = error "Can't remove from an empty list!"
        removeIndexes [0] (x:xs) = xs
        removeIndexes [n] (x:xs) = x:(removeIndexes [n-1] xs)
        removeIndexes (n:ns) xs = removeIndexes (fmap (subtract 1) ns) (removeIndexes [n] xs)
        
        subm :: (RemovedElements,RemovedElements) -> [[a]] -> [[a]]
        subm coords = map (removeIndexes (fst coords)) . removeIndexes (snd coords)
        
        supportsColsRaw = zip [0..] . init . subsequences $ [0..w] -- [[],[0],[1],...,[n],[0,1],[0,2],...,[0,n],[0,1,2],...]
        supportsRowsRaw = zip [0..] . init . subsequences $ [0..h]
        supportsPairs = map (\(x,c) -> map (\(y,r) -> ((c,r), (subm (c,r) m1, subm (c,r) m2))) supportsRowsRaw) supportsColsRaw
    in
        supportsPairs

gameSupportsZeroSum :: (Ord a, Num a) => [[a]] -> [[((RemovedElements, RemovedElements), ([[a]], [[a]]))]]
gameSupportsZeroSum m = gameSupports m (map (map negate) m)

supportIntoValidatedWeightsEVsSDs :: ((RemovedElements, RemovedElements), ([[Double]], [[Double]])) -> Maybe ((Weights, Weights), ((EV, EV), (SD, SD)))
supportIntoValidatedWeightsEVsSDs ((sc,sr), (mc,mr)) =
    do
        let (g1,g2) = (gameSimplePartial mc, tr' $ gameSimplePartial mr)
        w1 <- solveSimpleCoreNaive g1
        w2 <- solveSimpleCoreNaive g2
        let ws = (addIndexes 0 sc w1, addIndexes 0 sr w2)
        let evs = (calcEVSimpleCore g1 w1 w2, calcEVSimpleCore g2 w2 w1)
        let sds = (calcSDSimpleCore g1 w1 w2, calcSDSimpleCore g2 w2 w1)
        return (ws, (evs, sds))
    where
        addIndexes :: a -> [Int] -> [a] -> [a]
        addIndexes _ [] xs = xs
        addIndexes def [0] xs = def:xs
        addIndexes _ _ [] = error "Add index out of bounds"
        addIndexes def [n] (x:xs) = x:(addIndexes def [n-1] xs)
        addIndexes def (n:ns) xs = addIndexes def ns (addIndexes def [n] xs)


supportsIntoValidatedWeightsEVsSDs :: [[((RemovedElements, RemovedElements), ([[Double]], [[Double]]))]] -> [[Maybe ((Weights, Weights), ((EV, EV), (SD, SD)))]]
supportsIntoValidatedWeightsEVsSDs = map (map supportIntoValidatedWeightsEVsSDs)

solveSimple :: [[Double]] -> [[Double]] -> ResultSimple
solveSimple mc mr =
    let
        sups = gameSupports mc mr
        strats = supportsIntoValidatedWeightsEVsSDs sups
        
        maximumOn f = maximumBy (compare `on` f)
        colEv = fmap (fst . fst . snd)
        rowEv = fmap (snd . fst . snd)
        
        stratsBestResponses = map (maximumOn rowEv) strats
        stratsBest = maximumOn colEv stratsBestResponses
        
        res (Just ((wc,wr),((evc,evr),(sdc,sdr)))) = ResultSimple evc evr sdc sdr wc wr
    in
        res stratsBest

solveComplex :: Ord a => [((a, Maybe Double), (a, Maybe Double), Double, Double)] -> ResultSimple
solveComplex gdata =
    let
        typeCheck ((_,c), (_,r), _, _) = (fromEnum . isNothing $ c) + ((2*) . fromEnum . isNothing $ r)
        
        pureStrategy :: Int -> Int -> [Double]
        pureStrategy l x = take l ((take (x-1) . repeat $ 0) ++ (1:[0..]))
        --  get all the subgame data and split it according to type: neither = Nothing, col = Nothing, row = Nothing, both = Nothing
        --  evs :: [[((Text, Maybe Double), (Text, Maybe Double), Double)]]
        evs = map (\i -> filter (\outcome -> typeCheck outcome == i) $ gdata) [0..3]
    in
        case evs of
            [both, [], [], []]  -> do -- both are fixed, so we just copy the weights into the ResultSimple
                let rows = nub . map (fst . fst4) $ both
                let cols = nub . map (fst . snd4) $ both
                let grid = transpose . chunksOf (length rows) $ both
                let outsc = map (map thd4) grid
                let outsr = map (map fth4) grid
                
                let gc = fromLists outsc
                let gr = fromLists outsr
                let cw = map (maybe (error "???") id . snd) . nub . map fst4 $ both
                let rw = map (maybe (error "???") id . snd) . nub . map snd4 $ both
                ResultSimple (calcEVSimpleCore gc cw rw) (calcEVSimpleCore gr cw rw) (calcSDSimpleCore gc cw rw) (calcSDSimpleCore gr cw rw) cw rw

            [[], c, [], []]      -> do -- typeCheck 1 -> columns unfixed, rows fixed, evaluate columns
                let cols = nub . map (fst . fst4) $ c -- each column option, in the form of name::Text
                let rows = map (\(a, Just b) -> (a,b)) . nub . map snd4 $ c -- each row option, in the form of (name, weight)::(Text, Double)
                let grid = transpose . chunksOf (length rows) $ c
                let outsc = map (map thd4) grid
                let outsr = map (map fth4) grid
                let gc = fromLists outsc
                let gr = fromLists outsr
                
                let colStrats = map (pureStrategy . length $ cols) [0..length cols] -- every pure strategy to try against the fixed row strategy
                let evs = map (\strat -> calcEVSimpleCore gc strat (map snd rows)) colStrats -- every ev
                
                let (cw, ev) = maximumBy (compare `on` snd) . zip colStrats $ evs -- get the optimal pure strategy for the column player
                let rw = map snd rows
                ResultSimple ev (calcEVSimpleCore gr cw rw) (calcSDSimpleCore gc cw rw) (calcSDSimpleCore gr cw rw) cw rw

            [[], [], r, []]      -> do -- the same as above, but cols fixed rows unfixed
                let cols = map (\(a, Just b) -> (a,b)) . nub . map fst4 $ r
                let rows = nub . map (fst . snd4) $ r
                let grid = transpose . chunksOf (length rows) $ r
                let outsc = map (map thd4) grid
                let outsr = map (map fth4) grid
                let gc = fromLists outsc
                let gr = fromLists outsr
                
                let rowStrats = map (pureStrategy . length $ rows) [0..length rows]
                let evs = map (\strat -> calcEVSimpleCore gr (map snd cols) strat) rowStrats
                
                let cw = map snd cols
                let (rw, ev) = minimumBy (compare `on` snd) . zip rowStrats $ evs -- the defender wants to minimise the ev
                ResultSimple (calcEVSimpleCore gc cw rw) ev (calcSDSimpleCore gc cw rw) (calcSDSimpleCore gr cw rw) cw rw

            [[], [], [], neither]     -> do -- both are unfixed, so we just calculate it as a normal GameSimple
                let cols = nub . map (fst . fst4) $ neither
                let rows = nub . map (fst . snd4) $ neither
                let grid = transpose . chunksOf (length rows) $ neither
                let outsc = map (map thd4) grid
                let outsr = map (map fth4) grid
                
                solveSimple outsc outsr
                
            _                      -> error "both fixed and unfixed options for a single player" -- no. don't. this makes no sense!!! the only reason i can even think you'd want to try is equivalent to just nesting in another mixup, so just do that.

fst4 (x,_,_,_) = x
snd4 (_,x,_,_) = x
thd4 (_,_,x,_) = x
fth4 (_,_,_,x) = x
