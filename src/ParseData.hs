{-# LANGUAGE OverloadedStrings #-}

-- parses yaml into specific types also stored here
module ParseData
    ( ScoreData (ScoreData, scoreNameAtt, scoreNameDef, endName, updateName, outType, outPath) -- a structure containing all the data about scoring contexts, where to put output data, etc
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
import GameData

import Data.Text (Text, unpack)
import qualified Data.ByteString.Lazy as B
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Tree

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

data Instruction =
    Instruction { name::Text
                , path::Text
                , scores::[ScoreData]
                , context::Recontext
    } deriving (Eq, Ord, Show)

data Recontext =
    Recontext { colOption::Text
              , rowOption::Text
              , set::Context
              , add::Context
              , next::(Maybe MixupMetadata)
    } deriving (Eq, Ord, Show)

data MixupMetadata =
    MixupMetadata { metaName::Text
                  , metaAtt ::Text
                  , metaDef ::Text
    } deriving (Eq, Ord, Show)

data Option =
    Option { optionName::Text
           , require::Context
           , antirequire::Context
           , optionWeight::Maybe Double
    } deriving (Eq, Ord, Show)

type MixupData = Map MixupMetadata Mixup

data Mixup = 
    Mixup { mname :: Text
          , reqs::Context
          , antireqs::Context
          , attOptions::(Map Text Option)
          , defOptions::(Map Text Option)
          , outcomes::[Recontext]
    } deriving (Eq, Ord, Show)
