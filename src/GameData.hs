{-# LANGUAGE OverloadedStrings #-}

module GameData
    ( ResultSimple (ResultSimple, evSimpleCol, evSimpleRow, sdSimpleCol, sdSimpleRow, weightsColsSimple, weightsRowsSimple)
    , GameSimple (GameSimple, gameSName, gameSCols, gameSRows, gameSMatrixCol, gameSMatrixRow, gameSOutcomes)
    , Game (Game, gameCName, attCName, defCName, gameData, outcomesC)
    , Result (Result, evc, evr, sdc, sdr, weightsAtts, weightsDefs)
    , Opt
    , OptOutcome
    ) where

import Data.Function
import Data.List
import Data.Maybe
import Data.Text (Text, unpack)
import Numeric.LinearAlgebra hiding (build)
import Numeric.LinearAlgebra.Data hiding (build)
import Formatting
import Formatting.Formatters hiding (build)
import qualified Formatting.Formatters as F
import Formatting.Buildable

data ResultSimple = ResultSimple {evSimpleCol::Double, evSimpleRow::Double, sdSimpleCol::Double, sdSimpleRow::Double, weightsColsSimple::[Double], weightsRowsSimple::[Double]}
    deriving (Show)

data GameSimple = GameSimple {gameSName::Text, gameSCols::[Text], gameSRows::[Text], gameSMatrixCol::(Matrix Double), gameSMatrixRow::(Matrix Double), gameSOutcomes::ResultSimple}
instance Buildable GameSimple where
    build g = let
                  (ResultSimple evc evr sdc sdr wc wr) = gameSOutcomes g
                  n = gameSName g
                  m1 = gameSMatrixCol g
                  m2 = gameSMatrixRow g
                  c = zip (gameSCols g) wc
                  r = zip (gameSRows g) wr
              in
                  bformat (stext % ":\n Column's Game: " % shown % "\n Row's Game: " % shown % "\n Column player: " % shown % "\n Row player: " % shown % "\n EVs: " % shown % "\n SDs: " % shown) n m1 m2 c r (evc, evr) (sdc, sdr)
instance Show GameSimple where
    show g = formatToString F.build g

type Opt = (Text, Maybe Double)
type OptOutcome = (Opt, Opt, Double, Double)

data Result = Result {evc::Double, evr::Double, sdc::Double, sdr::Double, weightsAtts::[(Text,Double)], weightsDefs::[(Text,Double)]}
    deriving (Eq, Ord)
instance Show Result where
    show r = "\n EVs: " ++ (show (evc r, evr r)) ++ "\n SDs: " ++ (show (sdc r, sdr r)) ++ "\n Attacker Options: " ++ (show . weightsAtts $ r) ++ "\n Defender Options: " ++ (show . weightsDefs $ r)

data Game = Game {gameCName::Text, attCName::Text, defCName::Text, gameData::[OptOutcome], outcomesC::Result}
instance Ord Game where
    compare = compare `on` gameCName
instance Eq Game where
    a == b = and [gameCName a == gameCName b, attCName a == attCName b, defCName a == defCName b]
instance Show Game where
    show (Game "" _ _ gdata gout) = show gout
    show (Game gname "" "" gdata gout) = (unpack gname) ++ "\n" ++ (show gout)
    show (Game gname attname defname gdata gout) = (unpack gname) ++ " (" ++ (unpack attname) ++ " vs " ++ (unpack defname) ++ ")" ++ (show gout)
