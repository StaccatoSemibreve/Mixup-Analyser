module ScoreData
    ( Score
    , EndState
    , Updater
    , Printer
    , ModuleDatum (ModuleDatum, namedatum, mixdatum, scoreattdatum, scoredefdatum, enddatum, updatum, printdatum, printpath, outdatum)
    , ModuleData (ModuleData, mixdata, scoredata, enddata, updata, printdata, instrdata)
    , ModuleReader
    , FlagReader
    , Flags
    ) where

import Args
import ParseData
import Contexts

import Data.Text (Text)
import Control.Monad.Identity
import Data.Map (Map)
import Data.HashMap.Lazy (HashMap)
import Control.Monad.Reader

type Score = ContextS Double
type EndState = ContextS Bool
type Updater = ContextS ()
type Printer = TreeGame -> ModuleReader Identity Text

data ModuleDatum =
    ModuleDatum { namedatum :: Text
                , mixdatum :: MixupData
                , scoreattdatum :: [(Text, Score)]
                , scoredefdatum :: [(Text, Score)]
                , enddatum :: (Text, EndState)
                , updatum :: (Text, Updater)
                , printdatum :: (Text, Printer)
                , printpath :: Text
                , outdatum :: Recontext
    }
data ModuleData =
    ModuleData { mixdata :: HashMap Text MixupData
               , scoredata :: HashMap Text Score
               , enddata :: HashMap Text EndState
               , updata :: HashMap Text Updater
               , printdata :: HashMap Text Printer
               , instrdata :: [Instruction]
    }
type ModuleReader a = ReaderT (ModuleDatum, Flags) a
type FlagReader = ReaderT Flags IO
