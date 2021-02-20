{-# LANGUAGE OverloadedStrings #-}

module YAML
    ( printer
    ) where

import GameSolve
import ParseData
import Evaluate
import Contexts
import Score

import Data.Maybe
import Data.Text (Text, pack, unpack, append)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import Data.Tree
import Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as M
import Formatting
import Formatting.Formatters
import Formatting.Combinators
import Control.Monad.State.Lazy (evalState)
import Control.Monad.Reader (ReaderT)
import Control.Monad.Identity
import Data.YAML

instance ToYAML Result where
    toYAML r =
        let
            xs = [ ("Attacker EV"::Text, toYAML $ evc r)
                 , ("Defender EV", toYAML $ evr r)
                 , ("Attacker SD", toYAML $ sdc r)
                 , ("Defender SD", toYAML $ sdr r)
                 , ("Attacker Weights", toYAML . M.fromList $ weightsAtts r)
                 , ("Defender Weights", toYAML . M.fromList $ weightsDefs r) ]
        in
            toYAML (M.fromList xs)

instance ToYAML Game where
    toYAML g =
        let
            weird ((a,_),(b,_),c,d) = M.fromList [("Option Attacker"::Text, toYAML a),("Option Defender", toYAML b),("EV Attacker", toYAML c),("EV Defender", toYAML d)]
            
            xs = [ ("Game Name"::Text, toYAML $ gameCName g)
                 , ("Player Attacker", toYAML $ attCName g)
                 , ("Player Defender", toYAML $ defCName g)
                 , ("Option Data", toYAML . map weird $ gameData g)
                 , ("Result Data", toYAML $ outcomesC g) ]
        in
            toYAML (M.fromList xs)

instance ToYAML TreeGameItem where
    toYAML x =
        let
            xs = [ ("Context"::Text, toYAML $ tgiContext x)
                 , ("Option Attacker", toYAML . fst . tgiAtt $ x)
                 , ("Option Defender", toYAML . fst . tgiDef $ x)
                 , ("Game", toYAML $ tgiGame x)
                 , ("EVs Attacker", toYAML $ tgiAttEVs x)
                 , ("EVs Defender", toYAML $ tgiDefEVs x)
                 , ("SDs Attacker", toYAML $ tgiAttSDs x)
                 , ("SDs Defender", toYAML $ tgiDefSDs x) ]
        in
            toYAML (M.fromList xs)

instance (Ord a, ToYAML a) => ToYAML (Tree a) where
    toYAML (Node a []) = toYAML a
    toYAML (Node a xs) = toYAML (M.fromList [(a, xs)])

instance ToYAML MixupMetadata where
    toYAML m =
        let
            xs = [ ("Mixup"::Text, toYAML $ metaName m)
                 , ("Attacker", toYAML $ metaAtt m)
                 , ("Defender", toYAML $ metaDef m) ]
        in
            toYAML (M.fromList xs)

instance ToYAML Recontext where
    toYAML r =
        let
            xs = [ ("Context Set"::Text, toYAML $ set r)
                 , ("Context Add", toYAML $ add r)
                 , ("Starting Point", toYAML $ next r) ]
        in
            toYAML (M.fromList xs)

instance ToYAML ModuleDatum where
    toYAML mods =
        let
            xs = [ ("Name"::Text, toYAML $ namedatum mods)
                 , ("Modules Score Attacker", toYAML . map fst $ scoreattdatum mods)
                 , ("Modules Score Defender", toYAML . map fst $ scoredefdatum mods)
                 , ("Module EndState", toYAML . fst $ enddatum mods)
                 , ("Module Updater", toYAML . fst $ updatum mods)
                 , ("Module Printer", toYAML . fst $ printdatum mods)
                 , ("Initial Conditions", toYAML $ outdatum mods) ]
        in
            toYAML (M.fromList xs)

printer :: Printer
printer tree = do
    mods <- askMods
    let yaml = encode1 . M.fromList $ [("Metadata"::Text, toYAML mods), ("Output", toYAML tree)]
    return . TL.toStrict . TLE.decodeUtf8 $ yaml
