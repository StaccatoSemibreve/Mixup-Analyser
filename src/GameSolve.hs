{-# LANGUAGE OverloadedStrings #-}

module GameSolve
    ( solvePure, solvePureBy, solvePureOn, solvePureZeroSum
    , solvePureOptimal, solvePureOptimalZeroSum
    , solveSimpleCoreNaive
    , calcEVSimpleCoreNaive
    , solveSimpleCore
    , solveSimple
    , solveComplexCore
    , solveComplex
    , resultComplex
    , Game (Game, gameCName, attCName, defCName, gameData, outcomesC)
    , Result (Result, evc, evr, sdc, sdr, weightsAtts, weightsDefs)
    , ResultSimple  (ResultSimple, evSimpleCol, evSimpleRow, sdSimpleCol, sdSimpleRow, weightsColsSimple, weightsRowsSimple)
    ) where 

import GameData
import Game

import Data.List
import Data.List.Split
import Data.Function
import Data.Maybe
import Data.Text (Text, unpack)
import Data.Map (Map)
import qualified Data.Map as Map
import Numeric.LinearAlgebra
import Numeric.LinearAlgebra.Data
import Safe

type EV = Double
type SD = Double
type Weights = [Double]
type Coord = (Int,Int)
type RemovedElements = [Int]

-- the fundamental algorithm, given some ordering method to know which values are better
solvePureBy :: (Eq a) => (a -> a -> Ordering) -> [[a]] -> [[a]] -> [Coord]
solvePureBy f mc mr = let
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
solvePureOptimalBy f ev mc mr = let
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

solveSimpleCore :: [[Double]] -> [[Double]] -> ResultSimple
solveSimpleCore mc mr =
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

-- first, get all possible combinations of supports (equivalent to cartesian product then groupBy) - note that these are "antisupports", they are taken away rather than included for ease of use with hmatrix
-- then convert to the respective matrices and solve
-- then filter by valid results, find the optimal defending result per possible attack, then the optimal attacking result based on this
{-solveSimpleCore :: Matrix Double -> Matrix Double -> ResultSimple
solveSimpleCore mc mr = let
                            evCol g = fromMaybe (-1000) . calcEVSimpleCoreNaive $ g
                            evRow g = fromMaybe (-1000) . calcEVSimpleCoreNaive $ g -- note that the utility of the transpose is equivalent
                            
                            -- supports = a matrix for every subgame, including the full game, but excluding the null game - after all, it might be better to exclude options! note that here they are represented as rows/columns to remove from the game matrix
                            supportsColsRaw = init . subsequences $ [0..(cols mc - 1)]
                            supportsRowsRaw = init . subsequences $ [0..(rows mc - 1)] -- we assume the row game is transposed such that its rows and columns are aligned with the column game's
                            supportsPairs = map (\c -> (c,supportsRowsRaw)) supportsColsRaw
                            -- list of pairs of the form (supportColsRaw, highest row utility supportRowsRaw wrt to the col support)
                            bestRowSupports = map (\(c,rs) -> (c, maximumBy (compare `on` (\r -> evRow . subms (c,r) $ mr)) rs)) supportsPairs
                            supportGames = map (\(c,r) -> ((c,r), (subms (c,r) mc, subms (r,c) mr))) bestRowSupports
                            supportGamesSolved = map (\((c,r),(g1,g2)) -> (((c,r),(g1,g2)),(fromMaybe [-1] $ solveSimpleCoreNaive g1, fromMaybe [-1] $ solveSimpleCoreNaive g2))) supportGames
                            -- can't do things negative times or more than 100% of the time, we're not quantum wavefunctions over here
                            validatedSupportGamesSolved = filter (\((_,_),(c,r)) -> and [all (<=1) c, all (>=0) c, all (<=1) r, all (>=0) r]) supportGamesSolved
                            -- now the column player picks the support they're happiest with, assuming the row player knows what this will be and will act accordingly
                            (((supportCol, supportRow), (gamecol, gamerow)), (colWPart, rowWPart)) = maximumBy (compare `on` (\((_,(g,_)),_) -> evCol g)) validatedSupportGamesSolved
                            colW = addIndexes 0 supportCol colWPart
                            rowW = addIndexes 0 supportRow rowWPart
                        in
                            if size mc /= size mr
                               then error "Unequal matrix sizes between equivalent column and row games."
                               else ResultSimple (calcEVSimpleCore mc colW rowW) (calcEVSimpleCore mr colW rowW) (calcSDSimpleCore mc colW rowW) (calcSDSimpleCore mr colW rowW) colW rowW
    where
        subms :: ([Int], [Int]) -> Matrix Double -> Matrix Double -- remove rows and columns from a matrix
        subms vals m = fromColumns . removeIndexes (fst vals) . toColumns . fromRows . removeIndexes (snd vals) . toRows $ m
            where
                removeIndexes :: [Int] -> [a] -> [a]
                removeIndexes [] xs = xs
                removeIndexes _ [] = error "Can't remove from an empty list!"
                removeIndexes [0] (x:xs) = xs
                removeIndexes [n] (x:xs) = x:(removeIndexes [n-1] xs)
                removeIndexes (n:ns) xs = removeIndexes (fmap (subtract 1) ns) (removeIndexes [n] xs)

        addIndexes :: a -> [Int] -> [a] -> [a]
        addIndexes _ [] xs = xs
        addIndexes def [0] xs = def:xs
        addIndexes _ _ [] = error "Add index out of bounds"
        addIndexes def [n] (x:xs) = x:(addIndexes def [n-1] xs)
        addIndexes def (n:ns) xs = addIndexes def ns (addIndexes def [n] xs)-}
       
solveSimple :: GameSimple -> GameSimple
solveSimple (GameSimple x1 x2 x3  m1 m2 _) = GameSimple x1 x2 x3 m1 m2 . Just $ solveSimpleCore (toLists m1) (toLists m2)

solveComplexCore :: Ord a => [((a, Maybe Double), (a, Maybe Double), Double, Double)] -> ResultSimple
solveComplexCore gdata = do
--  get all the subgame data and split it according to type: neither = Nothing, col = Nothing, row = Nothing, both = Nothing
--  evs :: [[((Text, Maybe Double), (Text, Maybe Double), Double)]]
    let evs = map (\i -> filter (\outcome -> typeCheck outcome == i) $ gdata) [0..3]
    
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
             
             solveSimpleCore outsc outsr
             
         _                      -> error "both fixed and unfixed options for a single player" -- no. don't. this makes no sense!!! the only reason i can even think you'd want to try is equivalent to just nesting in another mixup, so just do that.
    where
        typeCheck ((_,c), (_,r), _, _) = (fromEnum . isNothing $ c) + ((2*) . fromEnum . isNothing $ r)
        
        pureStrategy :: Int -> Int -> [Double]
        pureStrategy l x = take l ((take (x-1) . repeat $ 0) ++ (1:[0..]))

solveComplex :: Game -> Game
solveComplex gc@(Game gname attname defname gdata _) = Game gname attname defname gdata . Just . resultComplex gdata . solveComplexCore $ gdata

fst4 (x,_,_,_) = x
snd4 (_,x,_,_) = x
thd4 (_,_,x,_) = x
fth4 (_,_,_,x) = x
