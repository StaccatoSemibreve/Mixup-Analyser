-- calculates nash equilibriums, does related backend stuff
module Game
    ( game -- create a Game from the necessary input data
    , solve -- take a Game in, return a Game with a solved optimal nash equilibrium (better result than nashpy in at least one weird testcase!)
    , calcEV -- take a Game in and two sets of weights (one for the row player, one for the column player), return an EV - TODO: actually test this, make sure it works!
    ) where

import Data.List
import Data.Function
import Numeric.LinearAlgebra
import Numeric.LinearAlgebra.Data

data Result = Outcome {ev::Double, weightsCols::[Double], weightsRows::[Double]}
    deriving (Show)
instance Ord Result where
    compare = compare `on` ev
instance Eq Result where
    (==) = (==) `on` ev

data Game = Game {gameName::String, colNames::[String], rowNames::[String], gameMatrix::(Matrix Double), outcomes::(Maybe Result)}

instance Ord Game where
    compare = compare `on` gameName
instance Eq Game where
    (==) = (==) `on` gameName
instance Show Game where
    show (Game gname cnames rnames m Nothing) = gname ++ ":\n" ++ (show m) ++ "\nColumn player: " ++ (show cnames) ++ "\nRow player: " ++ (show rnames)
    show (Game gname cnames rnames m (Just (Outcome ev wc wr))) = gname ++ ":\n" ++ (show m) ++ "\nColumn player: " ++ (show . zip cnames $ wc) ++ "\nRow player: " ++ (show . zip rnames $ wr) ++ "\nEV: " ++ (show ev)

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
                 (1,1) -> Outcome (head . concat . toLists $ m) [1] [1]
                 (1,c) -> (\ev -> Outcome (snd ev) ((take (fst ev) (repeat 0)) ++ [1] ++ (take (c - fst ev - 1) (repeat 0))) [1]) $ maximumBy (compare `on` snd) . zip [0..] $ (concat . toLists $ m)
                 (r,1) -> (\ev -> Outcome (snd ev) [1] ((take (fst ev) (repeat 0)) ++ [1] ++ (take (r - fst ev - 1) (repeat 0)))) $ minimumBy (compare `on` snd) . zip [0..] $ (concat . toLists $ m)
                 _ -> do
                     let inverse = pinvTol 0.01 m
                     let ev = (1/) . sum . concat . toLists $ inverse
                     let weights1 = concat . toLists $ ((scalar ev) * inverse Numeric.LinearAlgebra.<> (((rows m)><1) (repeat (fromInteger 1))))
                     let weights2 = concat . toLists $ ((scalar ev) * (tr' inverse) Numeric.LinearAlgebra.<> (((cols m)><1) (repeat (fromInteger 1))))
                     Outcome ev weights1 weights2

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
        validate (Outcome _ w1 w2) = (all (>=0) w1) && (all (>=0) w2)
        tidy :: (([Int],[Int]), Result) -> Result
        tidy ((r, c), Outcome ev w1 w2) = Outcome ev (addIndexes 0 r $ w1) (addIndexes 0 c $ w2)

game :: String -> [String] -> [String] -> [[Double]] -> Game
game n c r m = Game n c r (fromLists m) Nothing

calcEV :: Game -> [Double] -> [Double] -> Double
calcEV (Game _ _ _ mat _) cols rows = head . head . toLists $ (row cols) Numeric.LinearAlgebra.<> mat Numeric.LinearAlgebra.<> (col rows)
