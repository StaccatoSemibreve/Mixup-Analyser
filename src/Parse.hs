{-# LANGUAGE OverloadedStrings #-}

-- parses yaml into specific types also stored here
module Parse
    ( readInstructions -- get every Instruction from run.yaml, so we can know where to start
    , parseData -- take a FileName, return all the relevant mixup data from the file it sends us to
    ) where

import Contexts
import GameData
import ParseData

import Data.YAML
import Data.Text (Text, unpack)
import qualified Data.ByteString.Lazy as B
import Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as HashMap
import Data.Hashable
import qualified Data.Map as Map
import Data.Tree

instance (Hashable a, Eq a, Ord a, FromYAML a, FromYAML b) => FromYAML (HashMap a b) where
    parseYAML = fmap (HashMap.fromList . Map.toList) . parseYAML

data MixupGroup =
    MixupGroup { attacker::Text
               , defender::Text
               , mixups::[ParsedMixup]
    } deriving (Eq, Ord, Show)

data ParsedMixup = 
    ParsedMixup { mixupNameParsed :: Text
                , requireMixupParsed::Context
                , antirequireMixupParsed::Context
                , attackerOptionsParsed::[Option]
                , defenderOptionsParsed::[Option]
                , outcomesParsed::[Recontext]
    } deriving (Eq, Ord, Show)

instance FromYAML ScoreData where
    parseYAML = withMap "ScoreData" $ \m -> ScoreData
        <$> m .:    "attacker"
        <*> m .:    "defender"
        <*> m .:    "endstate"
        <*> m .:    "updater"
        <*> m .:    "format"
        <*> m .:    "path"

instance FromYAML Instruction where
    parseYAML = withMap "Instruction" $ \m -> Instruction
        <$> m .: "name"
        <*> m .: "path"
        <*> m .: "score"
        <*> m .: "context"

instance FromYAML Recontext where
    parseYAML = withMap "Recontext" $ \m -> Recontext
        <$> m .:? "attackerOption"  .!= "None1"
        <*> m .:? "defenderOption"  .!= "None2"
        <*> m .:? "set"             .!= newContext
        <*> m .:? "add"             .!= newContext
        <*> m .:? "next"            .!= Nothing

instance FromYAML MixupMetadata where
    parseYAML = withMap "MixupMetadata" $ \m -> MixupMetadata
        <$> m .: "mixup"
        <*> m .: "attacker"
        <*> m .: "defender"

instance FromYAML MixupGroup where
    parseYAML = withMap "MixupGroup" $ \m -> MixupGroup
        <$> m .: "attacker"
        <*> m .: "defender"
        <*> m .: "mixups"

instance FromYAML ParsedMixup where
    parseYAML = withMap "Mixup" $ \m -> ParsedMixup
        <$> m .:    "name"
        <*> m .:?   "require"           .!= newContext
        <*> m .:?   "antirequire"       .!= newContext
        <*> m .:?   "attackerOptions"   .!= [Option "None1" newContext newContext (Just 1)]
        <*> m .:?   "defenderOptions"   .!= [Option "None2" newContext newContext (Just 1)]
        <*> m .:    "outcomes"

instance FromYAML Option where
    parseYAML = withMap "Option" $ \m -> Option
        <$> m .:    "name"
        <*> m .:?   "require"       .!= newContext
        <*> m .:?   "antirequire"   .!= newContext
        <*> m .:?   "weight"        .!= Nothing

-- read any yaml into any type, do not export because we only need specific types
readYAML :: (FromYAML a) => FilePath -> IO (a)
readYAML path = do
    raw <- B.readFile path
    case decode1 raw of
        Left (loc, emsg) -> do
            error (path ++ ":" ++ prettyPosWithSource loc raw " error" ++ emsg)
        Right stuff -> pure stuff

readInstructions :: String -> IO ([Instruction])
readInstructions = readYAML

parseData :: FilePath -> IO (MixupData)
parseData = fmap (\mixes -> HashMap.fromList $ concat . map (\((a,d),mgroup) -> map (\m -> (MixupMetadata (mixupNameParsed m) a d, unparsedMixup m)) . mixups $ mgroup) . zip (zip (map attacker mixes) (map defender mixes)) $ mixes) . readYAML . (++".yaml")

-- from here onward, we're transforming parsed data into more useful variants with maps and such

mapify :: (Hashable k, Eq k) => (v -> k) -> [v] -> HashMap k v
mapify f = HashMap.fromList . map (\v -> (f v, v))

unparsedMixup :: ParsedMixup -> Mixup
unparsedMixup (ParsedMixup name req unreq atts defs outs) = Mixup name req unreq (mapify optionName atts) (mapify optionName defs) outs
