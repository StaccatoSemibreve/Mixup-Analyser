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
    show (Game gname cnames rnames m (Just (Result ev sd wc wr))) = (unpack gname) ++ ":\n" ++ (show m) ++ "\nColumn player: " ++ (show . zip cnames $ wc) ++ "\nRow player: " ++ (show . zip rnames $ wr) ++ "\nEV: " ++ (show ev) ++ ", SD: " ++ (show sd)

data Result = Result {ev::Double, sd::Double, weightsCols::[Double], weightsRows::[Double]}
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
                 (1,1) -> Result (head . concat . toLists $ m) 0 [1] [1]
                 (1,c) -> (\ev -> Result (snd ev) 0 ((take (fst ev) (repeat 0)) ++ [1] ++ (take (c - fst ev - 1) (repeat 0))) [1]) $ maximumBy (compare `on` snd) . zip [0..] $ (concat . toLists $ m)
                 (r,1) -> (\ev -> Result (snd ev) 0 [1] ((take (fst ev) (repeat 0)) ++ [1] ++ (take (r - fst ev - 1) (repeat 0)))) $ minimumBy (compare `on` snd) . zip [0..] $ (concat . toLists $ m)
                 _ -> do
                     let inverse = pinvTol 0.01 m
                     let ev = (1/) . sum . concat . toLists $ inverse
                     let weights1 = concat . toLists $ ((scalar ev) * inverse Numeric.LinearAlgebra.<> (((rows m)><1) (repeat (fromInteger 1))))
                     let weights2 = concat . toLists $ ((scalar ev) * (tr' inverse) Numeric.LinearAlgebra.<> (((cols m)><1) (repeat (fromInteger 1))))
                     let sd = calcSD (Game "" [] [] m Nothing) weights1 weights2
                     Result ev sd weights1 weights2

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
        validate (Result _ _ w1 w2) = (all (>=0) w1) && (all (>=0) w2)
        tidy :: (([Int],[Int]), Result) -> Result
        tidy ((r, c), Result ev sd w1 w2) = Result ev sd (addIndexes 0 r $ w1) (addIndexes 0 c $ w2)

game :: Text -> [Text] -> [Text] -> [[Double]] -> Game
game n c r m = Game n c r (fromLists m) Nothing

calcEV :: Game -> [Double] -> [Double] -> Double
calcEV (Game _ _ _ mat _) cols rows = (head . head . toLists $ (row (fmap (/ sum rows) rows)) Numeric.LinearAlgebra.<> mat Numeric.LinearAlgebra.<> (col (fmap (/ sum cols) cols)))

calcVar :: Game -> [Double] -> [Double] -> Double
calcVar g@(Game a b c mat e) cols rows = calcEV (Game a b c (cmap (\x -> (x - calcEV g cols rows)**2) mat) e) cols rows

calcSD :: Game -> [Double] -> [Double] -> Double
calcSD g cols rows = sqrt $ calcVar g cols rows

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
         [both, [], [], []]  -> do -- both are fixed, so we just copy the weights into 
             let rows = nub . map (fst . fst3) $ both
             let cols = nub . map (fst . snd3) $ both
             let outs = map (map thd3) . transpose . chunksOf (length rows) $ both
             
             let g      = game gname rows cols outs
             let cw     = map (maybe (error "???") id . snd) . nub . map fst3 $ both
             let rw     = map (maybe (error "???") id . snd) . nub . map snd3 $ both
             GameComplex gname attname defname gdata . Just . resultComplex gdata $ Result (calcEV g cw rw) (calcSD g cw rw) cw rw

         [[], c, [], []]      -> do -- typeCheck 1 -> columns unfixed, rows fixed, evaluate columns
             let cols = nub . map (fst . fst3) $ c -- each column option, in the form of name::Text
             let rows = map (\(a, Just b) -> (a,b)) . nub . map snd3 $ c -- each row option, in the form of (name, weight)::(Text, Double)
             let outs = transpose . map (map thd3) . chunksOf (length rows) $ c -- the outcome matrix
             let g = game gname cols (map fst rows) outs -- the game to evaluate evs with
             
             let colStrats = map (pureStrategy . length $ cols) [0..length cols] -- every pure strategy to try against the fixed row strategy
             let evs = map (\strat -> calcEV g strat (map snd rows)) colStrats -- every ev
             
             let (cw, ev) = maximumBy (compare `on` snd) . zip colStrats $ evs -- get the optimal pure strategy
             let rw = map snd rows
             let sd = calcSD g cw rw -- get the standard deviation
             GameComplex gname attname defname gdata . Just . resultComplex gdata $ Result ev sd cw rw

         [[], [], r, []]      -> do -- the same as above, but cols fixed rows unfixed
             let cols = map (\(a, Just b) -> (a,b)) . nub . map fst3 $ r
             let rows = nub . map (fst . snd3) $ r
             let outs = transpose . map (map thd3) . chunksOf (length rows) $ r
             let g = game gname (map fst cols) rows outs
             
             let rowStrats = map (pureStrategy . length $ rows) [0..length rows]
             let evs = map (\strat -> calcEV g (map snd cols) strat) rowStrats
             
             let cw = map snd cols
             let (rw, ev) = minimumBy (compare `on` snd) . zip rowStrats $ evs -- the defender wants to minimise the ev
             let sd = calcSD g cw rw
             GameComplex gname attname defname gdata . Just . resultComplex gdata $ Result ev sd cw rw

         [[], [], [], neither]     -> do -- both are unfixed, so we just calculate it as a normal Game
             let cols = nub . map (fst . fst3) $ neither
             let rows = nub . map (fst . snd3) $ neither
             let outs = map (map thd3) . transpose . chunksOf (length rows) $ neither
             
             let g      = solve $ game gname rows cols outs
             let res    = maybe (error "???") id . outcomes $ g
             GameComplex gname attname defname gdata . Just . resultComplex gdata $ res
             
         _                      -> error "both fixed and unfixed options for a single player" -- no. don't. this makes no sense!!! the only reason i can even think you'd want to try is equivalent to just nesting in another mixup, so just do that.
    
    where
        typeCheck ((_,c), (_,r), _) = (fromEnum . isNothing $ c) + ((2*) . fromEnum . isNothing $ r)
        
        pureStrategy :: Int -> Int -> [Double]
        pureStrategy l x = take l ((take (x-1) . repeat $ 0) ++ (1:[0..]))

data ResultComplex = ResultComplex {resCEV::Double, resCSD::Double, resCAtts::[(Text,Double)], resCDefs::[(Text,Double)]}
    deriving (Eq, Ord)

instance Show ResultComplex where
    show (ResultComplex ev sd atts defs) = "\n EV: " ++ (show ev) ++ "\n SD: " ++ (show sd) ++ "\n Attacker Options: " ++ (show atts) ++ "\n Defender Options: " ++ (show defs)

resultComplex :: [(Opt, Opt, Double)] -> Result -> ResultComplex
resultComplex opts (Result ev sd cw rw) = do
    let colnames = nub . map (fst . fst3) $ opts
    let rownames = nub . map (fst . snd3) $ opts
    ResultComplex ev sd (zip colnames cw) (zip rownames rw)

fst3 (x,_,_) = x
snd3 (_,x,_) = x
thd3 (_,_,x) = x
