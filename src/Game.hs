-- calculates nash equilibriums, does related backend stuff
{-# LANGUAGE OverloadedStrings #-}

module Game
    ( Game (gameName, colNames, rowNames, gameMatrix, outcomes)
    , Result (ev, weightsCols, weightsRows)
    , game -- create a Game from the necessary input data
    , solve -- take a Game in, return a Game with a solved optimal nash equilibrium (better result than nashpy in at least one weird testcase!)
    , calcEV -- take a Game in and two sets of weights (one for the row player, one for the column player), return an EV - TODO: actually test this, make sure it works!
    , GameComplex (gameCName, gameData, outcomesC)
    , gameComplex -- like a Game but has fixed weight options too
    , solveComplex
    , ResultComplex (resCEV, resCAtts, resCDefs)
    ) where

import Data.List
import Data.List.Split
import Data.Function
import Data.Maybe
import Data.Text (Text, unpack)
import Numeric.LinearAlgebra
import Numeric.LinearAlgebra.Data

data Game = Game {gameName::Text, colNames::[Text], rowNames::[Text], gameMatrix::(Matrix Double), outcomes::(Maybe Result)}

instance Ord Game where
    compare = compare `on` gameName
instance Eq Game where
    (==) = (==) `on` gameName
instance Show Game where
    show (Game gname cnames rnames m Nothing) = (unpack gname) ++ ":\n" ++ (show m) ++ "\nColumn player: " ++ (show cnames) ++ "\nRow player: " ++ (show rnames)
    show (Game gname cnames rnames m (Just (Result ev wc wr))) = (unpack gname) ++ ":\n" ++ (show m) ++ "\nColumn player: " ++ (show . zip cnames $ wc) ++ "\nRow player: " ++ (show . zip rnames $ wr) ++ "\nEV: " ++ (show ev)

data Result = Result {ev::Double, weightsCols::[Double], weightsRows::[Double]}
    deriving (Show)
instance Ord Result where
    compare = compare `on` ev
instance Eq Result where
    (==) = (==) `on` ev

-- first, get all possible combinations of supports (equivalent to cartesian product then groupBy) - note that these are "antisupports", they are taken away rather than included for ease of use with hmatrix
-- then convert to the respective matrices and solve
-- then filter by valid results, find the optimal defending result per possible attack, then the optimal attacking result based on this
solve :: Game -> Game
solve (Game x1 x2 x3 m _) = Game x1 x2 x3 m (Just (maximum . map (minimum) . map2 tidy . filter (not . null) . map (filter (validate . snd)) . map2 (\s -> (s, subsolve (subms s m))) . supports $ m))
    where
        map2 f = map (map f)
        
        supports :: Matrix Double -> [[([Int], [Int])]]
        supports m = map (\c -> map (\r -> (c,r)) . init . subsequences $ [0..(rows m - 1)]) $ (init . subsequences $ ([0..(cols m - 1)]))

        subsolve :: Matrix Double -> Result
        subsolve m =
            case (rows m, cols m) of
                 (1,1) -> Result (head . concat . toLists $ m) [1] [1]
                 (1,c) -> (\ev -> Result (snd ev) ((take (fst ev) (repeat 0)) ++ [1] ++ (take (c - fst ev - 1) (repeat 0))) [1]) $ maximumBy (compare `on` snd) . zip [0..] $ (concat . toLists $ m)
                 (r,1) -> (\ev -> Result (snd ev) [1] ((take (fst ev) (repeat 0)) ++ [1] ++ (take (r - fst ev - 1) (repeat 0)))) $ minimumBy (compare `on` snd) . zip [0..] $ (concat . toLists $ m)
                 _ -> do
                     let inverse = pinvTol 0.01 m
                     let ev = (1/) . sum . concat . toLists $ inverse
                     let weights1 = concat . toLists $ ((scalar ev) * inverse Numeric.LinearAlgebra.<> (((rows m)><1) (repeat (fromInteger 1))))
                     let weights2 = concat . toLists $ ((scalar ev) * (tr' inverse) Numeric.LinearAlgebra.<> (((cols m)><1) (repeat (fromInteger 1))))
                     Result ev weights1 weights2

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

        validate :: Result -> Bool
        validate (Result _ w1 w2) = (all (>=0) w1) && (all (>=0) w2)
        tidy :: (([Int],[Int]), Result) -> Result
        tidy ((r, c), Result ev w1 w2) = Result ev (addIndexes 0 r $ w1) (addIndexes 0 c $ w2)

game :: Text -> [Text] -> [Text] -> [[Double]] -> Game
game n c r m = Game n c r (fromLists m) Nothing

calcEV :: Game -> [Double] -> [Double] -> Double
calcEV (Game _ _ _ mat _) cols rows = head . head . toLists $ (row rows) Numeric.LinearAlgebra.<> mat Numeric.LinearAlgebra.<> (col cols)

type Opt = (Text, Maybe Double)
data GameComplex = GameComplex {gameCName::Text, attCName::Text, defCName::Text, gameData::[(Opt, Opt, Double)], outcomesC::(Maybe ResultComplex)}

instance Ord GameComplex where
    compare = compare `on` gameCName
instance Eq GameComplex where
    a == b = and [gameCName a == gameCName b, attCName a == attCName b, defCName a == defCName b]
instance Show GameComplex where
    show (GameComplex "" _ _ gdata Nothing) = show gdata
    show (GameComplex "" _ _ gdata (Just gout)) = show gout
    show (GameComplex gname "" "" gdata Nothing) = (unpack gname) ++ "\n" ++ (show gdata)
    show (GameComplex gname "" "" gdata (Just gout)) = (unpack gname) ++ "\n" ++ (show gout)
    show (GameComplex gname attname defname gdata Nothing) = (unpack gname) ++ " (" ++ (unpack attname) ++ " vs " ++ (unpack defname) ++ ")\n" ++ (show gdata)
    show (GameComplex gname attname defname gdata (Just gout)) = (unpack gname) ++ " (" ++ (unpack attname) ++ " vs " ++ (unpack defname) ++ ")\n" ++ (show gout)

gameComplex :: Text -> Text -> Text -> [(Opt, Opt, Double)] -> GameComplex
gameComplex t1 t2 t3 m = GameComplex t1 t2 t3 (sort m) Nothing

-- solveComplex :: GameComplex -> [[((Text, Maybe Double), (Text, Maybe Double), Double)]]
solveComplex gc@(GameComplex gname attname defname gdata _) = do
--  get all the subgame data and split it according to type: neither = Nothing, col = Nothing, row = Nothing, both = Nothing
--  evs :: [[((Text, Maybe Double), (Text, Maybe Double), Double)]]
    let evs = map (\i -> filter (\outcome -> typeCheck outcome == i) $ gdata) [0..3]
    
    case evs of
         [both, [], [], []]  -> do
             let rows = nub . map (fst . fst3) $ both
             let cols = nub . map (fst . snd3) $ both
             let outs = map (map thd3) . transpose . chunksOf (length rows) $ both
             
             let g      = game gname rows cols outs
             let cw     = map (maybe (error "???") id . snd) . nub . map fst3 $ both
             let rw     = map (maybe (error "???") id . snd) . nub . map snd3 $ both
             GameComplex gname attname defname gdata . Just . resultComplex gdata $ Result (calcEV g cw rw) cw rw

         [[], col, [], []]      -> do -- whoops col and row are the wrong way round let's just pretend it's fine, the code works if i swap the weights at the end at least
             let rows = nub . map (fst . fst3) $ col -- each column option, in the form of name::Text
             let cols = map (\(a, Just b) -> (a,b)) . nub . map snd3 $ col -- each row option, in the form of (name, weight)::(Text, Double)
             let outs = map (map thd3) . chunksOf (length rows) $ col -- the outcome matrix, also to zip per row
             let colsWeird = zip cols outs
             let gs = map (\(col, outs) -> solve $ game gname rows [fst col] [outs]) colsWeird -- solved games
             
             let g = game gname rows (map fst cols) outs
             let rowsNew = map (weightsCols . fromMaybe (error "???") . outcomes) $ gs
             let rowsEvs = map ((flip (calcEV g)) (map snd cols)) rowsNew
             
             let (rw, ev) = maximumBy (compare `on` snd) . zip rowsNew $ rowsEvs
             let cw = map snd cols
             GameComplex gname attname defname gdata . Just . resultComplex gdata $ Result ev rw cw

         [[], [], row, []]      -> do
             let rows = map (\(a, Just b) -> (a,b)) . nub . map fst3 $ row -- each row option, in the form of (name, weight)::(Text, Double)
             let cols = nub . map (fst . snd3) $ row -- each column option, in the form of name::Text
             let outsT = map (map thd3) . chunksOf (length rows) $ row -- the outcome matrix's transpose, because we need to zip it per column
             let rowsWeird = zip rows outsT
             let gs = map (\(row, outs) -> solve $ game gname [fst row] cols (transpose [outs])) rowsWeird -- solved games
             
             let outs = transpose outsT
             let g = game gname cols (map fst rows) outs
             let colsNew = map (weightsRows . fromMaybe (error "???") . outcomes) $ gs
             let colsEvs = map (calcEV g (map snd rows)) colsNew
             
             let (cw, ev) = minimumBy (compare `on` snd) . zip colsNew $ colsEvs
             let rw = map snd rows
             GameComplex gname attname defname gdata . Just . resultComplex gdata $ Result ev rw cw

         [[], [], [], neither]     -> do
             let cols = nub . map (fst . fst3) $ neither
             let rows = nub . map (fst . snd3) $ neither
             let outs = map (map thd3) . transpose . chunksOf (length rows) $ neither
             
             let g      = solve $ game gname rows cols outs
             let res    = maybe (error "???") id . outcomes $ g
             GameComplex gname attname defname gdata . Just . resultComplex gdata $ res
             
         _                      -> error "both fixed and unfixed options for a single player"
    
    where
        typeCheck ((_,c), (_,r), _) = (fromEnum . isNothing $ c) + ((2*) . fromEnum . isNothing $ r)

data ResultComplex = ResultComplex {resCEV::Double, resCAtts::[(Text,Double)], resCDefs::[(Text,Double)]}
    deriving (Eq, Ord)

instance Show ResultComplex where
    show (ResultComplex ev atts defs) = "\n EV: " ++ (show ev) ++ "\n Attacker Options: " ++ (show atts) ++ "\n Defender Options: " ++ (show defs)

resultComplex :: [(Opt, Opt, Double)] -> Result -> ResultComplex
resultComplex opts (Result ev cw rw) = do
    let colnames = nub . map (fst . fst3) $ opts
    let rownames = nub . map (fst . snd3) $ opts
    ResultComplex ev (zip colnames cw) (zip rownames rw)

fst3 (x,_,_) = x
snd3 (_,x,_) = x
thd3 (_,_,x) = x
