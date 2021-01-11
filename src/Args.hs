module Args
    ( opts
    , Flags (flagVerbose, flagHelp, flagConfig, flagLog, flagIn, flagOut, flagScores, flagUpdaters, flagEndStates, flagPrinters)
    , flags
    ) where

import System.Console.GetOpt
import System.Environment
import System.Exit
import System.IO
import Data.Maybe
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map

data Flag = Verbose
          | Help
          | Config String
          | Log String
          | In String
          | Out String
          | Scores String
          | Updaters String
          | EndStates String
          | Printers String
          deriving (Show, Eq, Ord)

options :: [OptDescr Flag]
options = [ Option ['v']    ["verbose"]     (NoArg Verbose)             "Output the log to stdout as it is created."
          , Option ['h']    ["help"]        (NoArg Help)                "Use this tool to create descriptions of probabilistically unexploitable play for the given mixups."
          , Option ['c']    ["config"]      (OptArg config "FILE")      "The configuration file - 'config.yaml' by default."
          , Option ['l']    ["log"]         (OptArg logf "FILE")        "The log file - 'log.txt' by default."
          , Option ['i']    ["in"]          (OptArg ind "DIR")          "The mixup data directory - 'in' by default."
          , Option ['o']    ["out"]         (OptArg outd "DIR")         "The output file directory - 'out' by default."
          , Option ['s']    ["scores"]      (OptArg scoresd "DIR")      "The Score Module directory, for rating how good or bad situations are - 'score' by default."
          , Option ['u']    ["updaters"]    (OptArg updatersd "DIR")    "The Updater Module directory, for managing relations between context values - 'updater' by default."
          , Option ['e']    ["endstates"]   (OptArg endstatesd "DIR")   "The EndState Module directory, for checking if the game has ended - 'endstate' by default."
          , Option ['p']    ["printers"]    (OptArg printersd "DIR")    "The Printer Module directory, for converting output data into a readable file - 'printer' by default."
          ]

config, logf, ind, outd, scoresd, updatersd, endstatesd, printersd :: Maybe String -> Flag
config      = Config    . fromMaybe "config.yaml"
logf        = Log       . fromMaybe "log.txt"
ind         = In        . fromMaybe "in"
outd        = Out       . fromMaybe "out"
scoresd     = Scores    . fromMaybe "score"
updatersd   = Updaters  . fromMaybe "updater"
endstatesd  = EndStates . fromMaybe "endstates"
printersd   = Printers  . fromMaybe "printer"

opts :: [String] -> IO ([Flag], [String])
opts argv = case getOpt Permute options argv of
                 (args,fs,[])   -> do
                     if Help `elem` args
                        then do putStrLn (usageInfo header options)
                                exitWith ExitSuccess
                        else return (args, fs)
                 (_,_,errs) -> ioError . userError $ concat errs ++ usageInfo header options
                where
                    header = "Usage: ic [OPTION...] files..."

isVerbose Verbose = True
isVerbose _ = False
isHelp Help = True
isHelp _ = False
isConfig (Config _) = True
isConfig _ = False
isLog (Log _) = True
isLog _ = False
isIn (In _) = True
isIn _ = False
isOut (Out _) = True
isOut _ = False
isScores (Scores _) = True
isScores _ = False
isUpdaters (Updaters _) = True
isUpdaters _ = False
isEndStates (EndStates _) = True
isEndStates _ = False
isPrintsrs (Printers _) = True
isPrinters _ = False

fromFlag (Config x) = x
fromFlag (Log x) = x
fromFlag (In x) = x
fromFlag (Out x) = x
fromFlag (Scores x) = x
fromFlag (Updaters x) = x
fromFlag (EndStates x) = x
fromFlag (Printers x) = x

flagValue :: (Flag -> Bool) -> String -> [Flag] -> String
flagValue fb def = maybe def fromFlag . find fb
hasVerbose       = any isVerbose
hasHelp          = any isHelp
getConfig        = flagValue isConfig "config.yaml"
getLog           = flagValue isLog "log.txt"
getIn            = flagValue isIn "in"
getOut           = flagValue isOut "out"
getScores        = flagValue isScores "score"
getUpdaters      = flagValue isUpdaters "updater"
getEndStates     = flagValue isEndStates "endstate"
getPrinters      = flagValue isPrinters "printer"

data Flags =
    Flags { flagVerbose     :: Bool
          , flagHelp        :: Bool
          , flagConfig      :: FilePath
          , flagLog         :: FilePath
          , flagIn          :: FilePath
          , flagOut         :: FilePath
          , flagScores      :: FilePath
          , flagUpdaters    :: FilePath
          , flagEndStates   :: FilePath
          , flagPrinters    :: FilePath
    }
flags :: [Flag] -> Flags
flags xs = Flags (hasVerbose xs) (hasHelp xs) (getConfig xs) (getLog xs) (getIn xs) (getOut xs) (getScores xs) (getUpdaters xs) (getEndStates xs) (getPrinters xs)
