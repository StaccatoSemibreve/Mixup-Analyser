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

data GameSimple = GameSimple {gameSName::Text, gameSCols::[Text], gameSRows::[Text], gameSMatrixCol::(Matrix Double), gameSMatrixRow::(Matrix Double), gameSOutcomes::(Maybe ResultSimple)}
instance Buildable GameSimple where
    build g = let
                  basebuilder = bformat (stext % ":\n Column's Game: " % shown % "\n Row's Game: " % shown % "\n Column player: " % shown % "\n Row player: " % shown)
                  resbuilder = bformat (stext % ":\n Column's Game: " % shown % "\n Row's Game: " % shown % "\n Column player: " % shown % "\n Row player: " % shown % "\n EVs: " % shown % "\n SDs: " % shown)
              in
                  case gameSOutcomes g of
                       Nothing                                      -> basebuilder (gameSName g) (gameSMatrixCol g) (gameSMatrixRow g) (gameSCols g)          (gameSRows g)
                       Just (ResultSimple evc evr sdc sdr wc wr)    -> resbuilder  (gameSName g) (gameSMatrixCol g) (gameSMatrixRow g) (zip (gameSCols g) wc) (zip (gameSRows g) wr) (evc, evr) (sdc, sdr)
instance Show GameSimple where
    show g = formatToString F.build g

type Opt = (Text, Maybe Double)
type OptOutcome = (Opt, Opt, Double, Double)

data Result = Result {evc::Double, evr::Double, sdc::Double, sdr::Double, weightsAtts::[(Text,Double)], weightsDefs::[(Text,Double)]}
    deriving (Eq, Ord)
instance Show Result where
    show r = "\n EVs: " ++ (show (evc r, evr r)) ++ "\n SDs: " ++ (show (sdc r, sdr r)) ++ "\n Attacker Options: " ++ (show . weightsAtts $ r) ++ "\n Defender Options: " ++ (show . weightsDefs $ r)

data Game = Game {gameCName::Text, attCName::Text, defCName::Text, gameData::[OptOutcome], outcomesC::(Maybe Result)}
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
