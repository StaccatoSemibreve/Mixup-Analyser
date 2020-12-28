{-# LANGUAGE OverloadedStrings #-}

-- parses yaml into specific types also stored here
module Parse
    ( readInstructions -- get every Instruction from run.yaml, so we can know where to start
    , parseData -- take a FileName, return all the relevant mixup data from the file it sends us to
    , ScoreData (ScoreData, scoreNameAtt, scoreNameDef, endName, updateName, outType, outPath) -- a structure containing all the data about scoring contexts, where to put output data, etc
    , Instruction (Instruction, name, path, scores, context) -- a structure containing a name (for output purposes i guess), the filepath of the mixup data, the specific score functions (ooh forgot about this, oh no i need to carry that through somehow), and the initial game context (as a Recontext, to be applied to an empty Context)
    , Context -- a lookup table of current game state, v important to track this!
    , Recontext (Recontext, colOption, rowOption, set, add, next) -- an interaction between both players that alters the Context in ways defined in set and add, and describes any subsequent mixups - the options listed are names, used to lookup the options from actual lists of Options
    , Mixup (Mixup, mname, reqs, antireqs, attOptions, defOptions, outcomes)
    , MixupData -- a map from (attacker, defender, mixup name) to mixups
    , Option (Option, optionName, optionWeight, require, antirequire) -- a specific option for one of the players to use, Maybe including a fixed weight (otherwise, the weight is calculated using Game)
    , MixupMetadata (MixupMetadata, metaName, metaAtt, metaDef) -- the next place to go after this mixup, Maybe listed in the Recontext under next
    , Opt
    , TreeContextItem
    , TreeContext
    , TreeGameItem
    , TreeGame
    , TreeScore
    ) where

import Contexts
import Game

import Data.YAML
import Data.Text (Text, unpack)
import qualified Data.ByteString.Lazy as B
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Tree

type Opt = (Text, Maybe Double)
type TreeContextItem = (Maybe MixupMetadata, Opt, Opt, Context)
type TreeContext = Tree TreeContextItem
type TreeGameItem = (Context, Opt, Opt, Game)
type TreeGame = Tree TreeGameItem
type TreeScore = Tree (Opt, Opt, Double)

data ScoreData =
    ScoreData { scoreNameAtt::Text
              , scoreNameDef::Text
              , endName::Text
              , updateName::Text
              , outType::Text
              , outPath::Text
    } deriving (Eq, Ord, Show)

instance FromYAML ScoreData where
    parseYAML = withMap "ScoreData" $ \m -> ScoreData
        <$> m .:    "attacker"
        <*> m .:    "defender"
        <*> m .:    "endstate"
        <*> m .:    "updater"
        <*> m .:    "format"
        <*> m .:    "path"

data Instruction =
    Instruction { name::Text
                , path::Text
                , scores::[ScoreData]
                , context::Recontext
    } deriving (Eq, Ord, Show)

instance FromYAML Instruction where
    parseYAML = withMap "Instruction" $ \m -> Instruction
        <$> m .: "name"
        <*> m .: "path"
        <*> m .: "score"
        <*> m .: "context"

data Recontext =
    Recontext { colOption::Text
              , rowOption::Text
              , set::Context
              , add::Context
              , next::(Maybe MixupMetadata)
    } deriving (Eq, Ord, Show)

instance FromYAML Recontext where
    parseYAML = withMap "Recontext" $ \m -> Recontext
        <$> m .:? "attackerOption"  .!= "None1"
        <*> m .:? "defenderOption"  .!= "None2"
        <*> m .:? "set"             .!= newContext
        <*> m .:? "add"             .!= newContext
        <*> m .:? "next"            .!= Nothing

data MixupMetadata =
    MixupMetadata { metaName::Text
                  , metaAtt ::Text
                  , metaDef ::Text
    } deriving (Eq, Ord, Show)

instance FromYAML MixupMetadata where
    parseYAML = withMap "MixupMetadata" $ \m -> MixupMetadata
        <$> m .: "mixup"
        <*> m .: "attacker"
        <*> m .: "defender"

data MixupGroup =
    MixupGroup { attacker::Text
               , defender::Text
               , mixups::[ParsedMixup]
    } deriving (Eq, Ord, Show)

instance FromYAML MixupGroup where
    parseYAML = withMap "MixupGroup" $ \m -> MixupGroup
        <$> m .: "attacker"
        <*> m .: "defender"
        <*> m .: "mixups"

data ParsedMixup = 
    ParsedMixup { mixupNameParsed :: Text
                , requireMixupParsed::Context
                , antirequireMixupParsed::Context
                , attackerOptionsParsed::[Option]
                , defenderOptionsParsed::[Option]
                , outcomesParsed::[Recontext]
    } deriving (Eq, Ord, Show)

instance FromYAML ParsedMixup where
    parseYAML = withMap "Mixup" $ \m -> ParsedMixup
        <$> m .:    "name"
        <*> m .:?   "require"           .!= newContext
        <*> m .:?   "antirequire"       .!= newContext
        <*> m .:?   "attackerOptions"   .!= [Option "None1" newContext newContext (Just 1)]
        <*> m .:?   "defenderOptions"   .!= [Option "None2" newContext newContext (Just 1)]
        <*> m .:    "outcomes"

data Option =
    Option { optionName::Text
           , require::Context
           , antirequire::Context
           , optionWeight::Maybe Double
    } deriving (Eq, Ord, Show)

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
parseData = fmap (\mixes -> Map.fromList $ concat . map (\((a,d),mgroup) -> map (\m -> (MixupMetadata (mixupNameParsed m) a d, unparsedMixup m)) . mixups $ mgroup) . zip (zip (map attacker mixes) (map defender mixes)) $ mixes) . readYAML . (++".yaml")

-- from here onward, we're transforming parsed data into more useful variants with maps and such

mapify :: Ord k => (v -> k) -> [v] -> Map k v
mapify f = Map.fromList . map (\v -> (f v, v))

type MixupData = Map MixupMetadata Mixup

data Mixup = 
    Mixup { mname :: Text
          , reqs::Context
          , antireqs::Context
          , attOptions::(Map Text Option)
          , defOptions::(Map Text Option)
          , outcomes::[Recontext]
    } deriving (Eq, Ord, Show)

unparsedMixup :: ParsedMixup -> Mixup
unparsedMixup (ParsedMixup name req unreq atts defs outs) = Mixup name req unreq (mapify optionName atts) (mapify optionName defs) outs
