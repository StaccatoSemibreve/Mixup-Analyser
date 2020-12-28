-- calculates nash equilibriums, does related backend stuff
{-# LANGUAGE OverloadedStrings #-}

module Game
    ( ResultSimple (evSimpleCol, evSimpleRow, sdSimpleCol, sdSimpleRow, weightsColsSimple, weightsRowsSimple)
    , Game (Game, gameCName, gameData, outcomesC)
    , gameComplex -- like a Game but has fixed weight options too
    , solveComplex
    , solveComplexCore
    , Result (Result, evc, evr, sdc, sdr, weightsAtts, weightsDefs)
    , resultComplex
    ) where

import Data.List
import Data.List.Split
import Data.Function
import Data.Maybe
import Data.Text (Text, unpack)
import Numeric.LinearAlgebra
import Numeric.LinearAlgebra.Data

data GameSimple = GameSimple {gameSName::Text, gameSCols::[Text], gameSRows::[Text], gameSMatrixCol::(Matrix Double), gameSMatrixRow::(Matrix Double), gameSOutcomes::(Maybe ResultSimple)}

instance Show GameSimple where
    show g = case gameSOutcomes g of
                  Nothing                   -> (unpack . gameSName $ g) ++ ":\nColumn's Game:" ++ (show . gameSMatrixCol $ g) ++ "\nRow's Game" ++ (show . gameSMatrixRow $ g) ++ "\nColumn player: " ++ (show . gameSCols $ g) ++ "\nRow player: " ++ (show . gameSRows $ g)
                  Just (ResultSimple evc evr sdc sdr wc wr) -> (unpack . gameSName $ g) ++ ":\nColumn's Game:" ++ (show . gameSMatrixCol $ g) ++ "\nRow's Game" ++ (show . gameSMatrixRow $ g) ++ "\nColumn player: " ++ (show . zip (gameSCols g) $ wc) ++ "\nRow player: " ++ (show . zip (gameSRows g) $ wr) ++ "\nEVs: " ++ (show (evc, evr)) ++ ", SDs: " ++ (show (sdc, sdr))

data ResultSimple = ResultSimple {evSimpleCol::Double, evSimpleRow::Double, sdSimpleCol::Double, sdSimpleRow::Double, weightsColsSimple::[Double], weightsRowsSimple::[Double]}
    deriving (Show)

-- first, get all possible combinations of supports (equivalent to cartesian product then groupBy) - note that these are "antisupports", they are taken away rather than included for ease of use with hmatrix
-- then convert to the respective matrices and solve
-- then filter by valid results, find the optimal defending result per possible attack, then the optimal attacking result based on this
solveSimpleCore :: Matrix Double -> Matrix Double -> ResultSimple
solveSimpleCore mc mr = let
                            -- supports = a matrix for every subgame, including the full game, but excluding the null game - after all, it might be better to exclude options! note that here they are represented as rows/columns to remove from the game matrix
                            supportsColsRaw = init . subsequences $ [0..(cols mc - 1)]
                            supportsRowsRaw = init . subsequences $ [0..(cols mr - 1)]
                            -- [(supportColsRaw, highest row utility supportRowsRaw wrt to the col support)]
                            bestRowSupports = map (\c -> (c, maximumBy (compare `on` (\r -> naiveev . subms (c,r) $ mr)) supportsRowsRaw)) supportsColsRaw
                            supportGames = map (\(c,r) -> ((c,r), (subms (c,r) mc, subms (c,r) mr))) bestRowSupports
                            supportGamesSolved = map (\((c,r),(g1,g2)) -> (((c,r),(g1,g2)),(naivesolve g1, naivesolve g2))) supportGames
                            -- can't do things negative times or more than 100% of the time, we're not quantum wavefunctions over here
                            validatedSupportGamesSolved = filter (\((_,_),(c,r)) -> and [all (<=1) c, all (>=0) c, all (<=1) r, all (>=0) r]) supportGamesSolved
                            -- now the column player picks the support they're happiest with, assuming the row player knows what this will be and will act accordingly
                            (((supportCol, supportRow), (gamecol, gamerow)), (colWPart, rowWPart)) = maximumBy (compare `on` (\((_,(g,_)),_) -> naiveev g)) validatedSupportGamesSolved
                            colW = addIndexes 0 supportCol colWPart
                            rowW = addIndexes 0 supportRow rowWPart
                        in
                            if size mc /= size mr
                               then error "Unequal matrix sizes between equivalent column and row games."
                               else ResultSimple (calcEVSimpleCore mc colW rowW) (calcEVSimpleCore mr colW rowW) (calcSDSimpleCore mc colW rowW) (calcSDSimpleCore mr colW rowW) colW rowW
    where
        naiveev :: Matrix Double -> Double
        naiveev m =
            case (rows m, cols m) of
                 (1,1)  -> head . head . toLists $ m
                 (_,1)  -> maximum . concat . toLists $ m
                 (1,_)  -> maximum . concat . toLists $ m
                 _      -> (1/) . sum . concat . toLists . pinvTol 0.01 $ m
        
        naivesolve :: Matrix Double -> [Double]
        naivesolve m =
            case (rows m, cols m) of
                 -- not really much choice
                 (_,1) -> [1]
                 -- they only have one option, so zip your options to their position, get the best option's position, and pad a [1] with zeroes as appropriate to the position
                 (1,c) -> (\n -> ((take n (repeat 0)) ++ [1] ++ (take (c - n - 1) (repeat 0)))) . fst . maximumBy (compare `on` snd) . zip [0..] . concat . toLists $ m
                 _ -> do
                     let inverse = pinvTol 0.01 m
                     let ev = (1/) . sum . concat . toLists $ inverse
                     let weights = concat . toLists $ ((scalar ev) * inverse Numeric.LinearAlgebra.<> (((rows m)><1) (repeat (fromInteger 1))))
                     weights

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
        addIndexes def (n:ns) xs = addIndexes def ns (addIndexes def [n] xs)

-- first, get all possible combinations of supports (equivalent to cartesian product then groupBy) - note that these are "antisupports", they are taken away rather than included for ease of use with hmatrix
-- then convert to the respective matrices and solve
-- then filter by valid results, find the optimal defending result per possible attack, then the optimal attacking result based on this
solveSimple :: GameSimple -> GameSimple
solveSimple (GameSimple x1 x2 x3  m1 m2 _) = GameSimple x1 x2 x3 m1 m2 . Just $ solveSimpleCore m1 m2

gameSimple :: Text -> [Text] -> [Text] -> [[Double]] -> [[Double]] -> GameSimple
gameSimple n c r m1 m2 = GameSimple n c r (fromLists m1) (fromLists m2) Nothing

calcEVSimpleCore :: Matrix Double -> [Double] -> [Double] -> Double
calcEVSimpleCore mat cols rows = (head . head . toLists $ (row (fmap (/ sum rows) rows)) Numeric.LinearAlgebra.<> mat Numeric.LinearAlgebra.<> (col (fmap (/ sum cols) cols)))

calcVarSimpleCore :: Matrix Double -> [Double] -> [Double] -> Double
calcVarSimpleCore mat cols rows = calcEVSimpleCore (cmap (\x -> (x - calcEVSimpleCore mat cols rows)**2) mat) cols rows

calcSDSimpleCore :: Matrix Double -> [Double] -> [Double] -> Double
calcSDSimpleCore a b = sqrt . calcVarSimpleCore a b

type Opt = (Text, Maybe Double)
data Game = Game {gameCName::Text, attCName::Text, defCName::Text, gameData::[(Opt, Opt, Double, Double)], outcomesC::(Maybe Result)}

instance Ord Game where
    compare = compare `on` gameCName
instance Eq Game where
    a == b = and [gameCName a == gameCName b, attCName a == attCName b, defCName a == defCName b]
instance Show Game where
    show (Game "" _ _ gdata Nothing) = show gdata
    show (Game "" _ _ gdata (Just gout)) = show gout
    show (Game gname "" "" gdata Nothing) = (unpack gname) ++ "\n" ++ (show gdata)
    show (Game gname "" "" gdata (Just gout)) = (unpack gname) ++ "\n" ++ (show gout)
    show (Game gname attname defname gdata Nothing) = (unpack gname) ++ " (" ++ (unpack attname) ++ " vs " ++ (unpack defname) ++ ")\n" ++ (show gdata)
    show (Game gname attname defname gdata (Just gout)) = (unpack gname) ++ " (" ++ (unpack attname) ++ " vs " ++ (unpack defname) ++ ")" ++ (show gout)

gameComplex :: Text -> Text -> Text -> [(Opt, Opt, Double, Double)] -> Game
gameComplex t1 t2 t3 m = Game t1 t2 t3 (sort m) Nothing

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
             let gc = fromLists outsc
             let gr = fromLists outsr
             
             solveSimpleCore gc gr
             
         _                      -> error "both fixed and unfixed options for a single player" -- no. don't. this makes no sense!!! the only reason i can even think you'd want to try is equivalent to just nesting in another mixup, so just do that.
    
    where
        typeCheck ((_,c), (_,r), _, _) = (fromEnum . isNothing $ c) + ((2*) . fromEnum . isNothing $ r)
        
        pureStrategy :: Int -> Int -> [Double]
        pureStrategy l x = take l ((take (x-1) . repeat $ 0) ++ (1:[0..]))

solveComplex :: Game -> Game
solveComplex gc@(Game gname attname defname gdata _) = Game gname attname defname gdata . Just . resultComplex gdata . solveComplexCore $ gdata

data Result = Result {evc::Double, evr::Double, sdc::Double, sdr::Double, weightsAtts::[(Text,Double)], weightsDefs::[(Text,Double)]}
    deriving (Eq, Ord)

instance Show Result where
    show r = "\n EVs: " ++ (show (evc r, evr r)) ++ "\n SDs: " ++ (show (sdc r, sdr r)) ++ "\n Attacker Options: " ++ (show . weightsAtts $ r) ++ "\n Defender Options: " ++ (show . weightsDefs $ r)

resultComplex :: [(Opt, Opt, Double, Double)] -> ResultSimple -> Result
resultComplex opts (ResultSimple evc evr sdc sdr cw rw) = do
    let colnames = nub . map (fst . fst4) $ opts
    let rownames = nub . map (fst . snd4) $ opts
    Result evc evr sdc sdr (zip colnames cw) (zip rownames rw)

fst3 (x,_,_) = x
snd3 (_,x,_) = x
thd3 (_,_,x) = x
fst4 (x,_,_,_) = x
snd4 (_,x,_,_) = x
thd4 (_,_,x,_) = x
fth4 (_,_,_,x) = x
