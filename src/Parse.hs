{-# LANGUAGE OverloadedStrings #-}

-- parses yaml into specific types also stored here
module Parse
    ( readInstructions -- get every Instruction from run.yaml, so we can know where to start
    , instructionToMixupGroups -- take an Instruction, return all the relevant mixup data from the file it sends us to
    , Instruction (Instruction, context) -- a structure containing a name (for output purposes i guess), the filepath of the mixup data, the specific score functions (ooh forgot about this, oh no i need to carry that through somehow), and the initial game context (as a Recontext, to be applied to an empty Context) TODO: output, use scores data
    , Context -- a lookup table of current game state, v important to track this!
    , Recontext (Recontext, attackerOption, defenderOption, set, add, next) -- an interaction between both players that alters the Context in ways defined in set and add, and describes any subsequent mixups - the options listed are names, used to lookup the options from actual lists of Options
    , Mixup (Mixup, mixupName) -- a raw mixup
    , MixupGroup (MixupGroup, attacker, defender, mixups) -- a list of all the mixups between a specific attacker and specific defender - usually, [MixupGroup] will be used to represent an entire mixup data file
    , Option (Option, optionName, optionWeight) -- a specific option for one of the players to use, Maybe including a fixed weight (otherwise, the weight is calculated using Game)
    , NextMixup (NextMixup) -- the next place to go after this mixup, Maybe listed in the Recontext under next
    ) where

import Data.YAML
import Data.Text (Text, unpack)
import qualified Data.ByteString.Lazy as B

data Instruction =
    Instruction { name::Text
                , path::Text
                , scores::[Text]
                , context::Recontext
    } deriving (Eq, Show)

instance FromYAML Instruction where
    parseYAML = withMap "Instruction" $ \m -> Instruction
        <$> m .: "name"
        <*> m .: "path"
        <*> m .: "score"
        <*> m .: "context"

type Context = [(Text, Integer)]

data Recontext =
    Recontext { attackerOption::Text
              , defenderOption::Text
              , set::Context
              , add::Context
              , next::(Maybe NextMixup)
    } deriving (Eq, Show)

instance FromYAML Recontext where
    parseYAML = withMap "Recontext" $ \m -> Recontext
        <$> m .:? "attackerOption"  .!= "None1"
        <*> m .:? "defenderOption"  .!= "None2"
        <*> m .:? "set"             .!= []
        <*> m .:? "add"             .!= []
        <*> m .:? "next"            .!= Nothing

data NextMixup =
    NextMixup { nextAttacker::Text
              , nextDefender::Text
              , mixup::Text
    } deriving (Eq, Show)

instance FromYAML NextMixup where
    parseYAML = withMap "NextMixup" $ \m -> NextMixup
        <$> m .: "attacker"
        <*> m .: "defender"
        <*> m .: "mixup"

data MixupGroup =
    MixupGroup { attacker::Text
               , defender::Text
               , mixups::[Mixup]
    } deriving (Eq, Show)

instance FromYAML MixupGroup where
    parseYAML = withMap "MixupGroup" $ \m -> MixupGroup
        <$> m .: "attacker"
        <*> m .: "defender"
        <*> m .: "mixups"

data Mixup = 
    Mixup { mixupName :: Text
          , requireMixup::Context
          , antirequireMixup::Context
          , attackerOptions::[Option]
          , defenderOptions::[Option]
          , outcomes::[Recontext]
    } deriving (Eq, Show)

instance FromYAML Mixup where
    parseYAML = withMap "Mixup" $ \m -> Mixup
        <$> m .:    "name"
        <*> m .:?   "require"           .!= []
        <*> m .:?   "antirequire"       .!= []
        <*> m .:?   "attackerOptions"   .!= [Option "None1" [] [] (Just 1)]
        <*> m .:?   "defenderOptions"   .!= [Option "None2" [] [] (Just 1)]
        <*> m .:    "outcomes"

data Option =
    Option { optionName::Text
           , require::Context
           , antirequire::Context
           , optionWeight::Maybe Double
    } deriving (Eq, Show)

instance FromYAML Option where
    parseYAML = withMap "Option" $ \m -> Option
        <$> m .:    "name"
        <*> m .:?   "require"       .!= []
        <*> m .:?   "antirequire"   .!= []
        <*> m .:?   "weight"        .!= Nothing

-- read any yaml into any type, do not export because we only need specific types
readYAML :: (FromYAML a) => FilePath -> IO (a)
readYAML path = do
    raw <- B.readFile path
    case decode1 raw of
        Left (loc, emsg) -> do
            error (path ++ ":" ++ prettyPosWithSource loc raw " error" ++ emsg)
        Right stuff -> pure stuff

readInstructions :: IO ([Instruction])
readInstructions = readYAML "run.yaml"

instructionToMixupGroups :: Instruction -> IO ([MixupGroup])
instructionToMixupGroups = readYAML . unpack . path