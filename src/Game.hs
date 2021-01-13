-- calculates nash equilibriums, does related backend stuff
{-# LANGUAGE OverloadedStrings #-}

module Game
    ( gameSimplePartial, gameSimplePartialOpts, gameSimple
    , calcPureEVs, calcEVSimpleCore, calcVarSimpleCore, calcSDSimpleCore
    , gameComplex, resultComplex
    ) where

import GameData

import Data.List
import Data.List.Split
import Data.Function
import Data.Maybe
import Data.Text (Text, unpack)
import Numeric.LinearAlgebra
import Numeric.LinearAlgebra.Data

gameSimplePartial :: [[Double]] -> Matrix Double
gameSimplePartial = fromLists

gameSimplePartialOpts :: [(Opt, Opt, Double)] -> [[Double]]
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

gameSimple :: Text -> [Text] -> [Text] -> [[Double]] -> [[Double]] -> GameSimple
gameSimple n c r m1 m2 = GameSimple n c r (fromLists m1) (fromLists m2) Nothing

calcPureEVs :: Matrix Double -> [Double] -> [Double]
calcPureEVs m cols = concat . toLists $ m Numeric.LinearAlgebra.<> (col (fmap (/ sum cols) cols))

calcEVSimpleCore :: Matrix Double -> [Double] -> [Double] -> Double
calcEVSimpleCore m cols rows = (head . head . toLists $ (row rows) Numeric.LinearAlgebra.<> m Numeric.LinearAlgebra.<> (col cols))

-- calcEVSimple :: GameSimple -> ResultSimple -> Double

calcVarSimpleCore :: Matrix Double -> [Double] -> [Double] -> Double
calcVarSimpleCore mat cols rows = calcEVSimpleCore (cmap (\x -> (x - calcEVSimpleCore mat cols rows)**2) mat) cols rows

calcSDSimpleCore :: Matrix Double -> [Double] -> [Double] -> Double
calcSDSimpleCore a b = sqrt . calcVarSimpleCore a b

gameComplex :: Text -> Text -> Text -> [(Opt, Opt, Double, Double)] -> Game
gameComplex t1 t2 t3 m = Game t1 t2 t3 (sort m) Nothing

resultComplex :: [(Opt, Opt, Double, Double)] -> ResultSimple -> Result
resultComplex opts (ResultSimple evc evr sdc sdr cw rw) = do
    let colnames = nub . map (fst . fst4) $ opts
    let rownames = nub . map (fst . snd4) $ opts
    Result evc evr sdc sdr (zip colnames cw) (zip rownames rw)

fst4 (x,_,_,_) = x
snd4 (_,x,_,_) = x
thd4 (_,_,x,_) = x
fth4 (_,_,_,x) = x
