module Score
    ( Score
    , EndState
    , Updater
    , Printer
    , ModuleDatum (ModuleDatum, namedatum, mixdatum, scoreattdatum, scoredefdatum, enddatum, updatum, printdatum, printpath, outdatum)
    , ModuleData (ModuleData, mixdata, scoredata, enddata, updata, printdata, instrdata)
    , ModuleReader
    , FlagReader
    , environment
    , getModule
    , logger
    , writer
    ) where

import Contexts
import Parse
import Args

import Data.Maybe
import Control.Monad
import Data.List
import Data.List.Split
import Data.Text (Text, pack, unpack)
import System.Directory (listDirectory, createDirectoryIfMissing)
import System.FilePath (makeValid)
import Language.Haskell.Interpreter
import qualified Language.Haskell.Interpreter as I
import Formatting
import Formatting.Formatters
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad.Reader

type Score = Context -> Double
type EndState = Context -> Bool
type Updater = Context -> Context
type Printer = TreeGame -> Text

parseScore :: FilePath -> String -> IO (Either InterpreterError Score)
parseScore dir name = runInterpreter $ do
                I.set [languageExtensions := [OverloadedStrings]]
                loadModules [dir ++ "/" ++ name ++ ".hs"]
                setTopLevelModules [name]
                setImportsF [ ModuleImport "Prelude" NotQualified NoImportList
                            , ModuleImport "Contexts" NotQualified NoImportList
                            , ModuleImport "Data.Text" NotQualified (ImportList ["Text"])
                            , ModuleImport "Data.Map" NotQualified (ImportList ["Map"])
                            ]
                
                interpret ("score") (as :: Score)

parseEndState :: FilePath -> String -> IO (Either InterpreterError EndState)
parseEndState dir name = runInterpreter $ do
                I.set [languageExtensions := [OverloadedStrings]]
                loadModules [dir ++ "/" ++ name ++ ".hs"]
                setTopLevelModules [name]
                setImportsF [ ModuleImport "Prelude" NotQualified NoImportList
                            , ModuleImport "Contexts" NotQualified NoImportList
                            , ModuleImport "Data.Text" NotQualified (ImportList ["Text"])
                            , ModuleImport "Data.Map" NotQualified (ImportList ["Map"])
                            ]
                
                interpret ("end") (as :: EndState)

parseUpdater :: FilePath -> String -> IO (Either InterpreterError Updater)
parseUpdater dir name = runInterpreter $ do
                I.set [languageExtensions := [OverloadedStrings]]
                loadModules [dir ++ "/" ++ name ++ ".hs"]
                setTopLevelModules [name]
                setImportsF [ ModuleImport "Prelude" NotQualified NoImportList
                            , ModuleImport "Contexts" NotQualified NoImportList
                            , ModuleImport "Data.Text" NotQualified (ImportList ["Text"])
                            , ModuleImport "Data.Map" NotQualified (ImportList ["Map"])
                            ]
                
                interpret ("update") (as :: Updater)

parsePrinter :: FilePath -> String -> IO (Either InterpreterError Printer)
parsePrinter dir name = runInterpreter $ do
                I.set [languageExtensions := [OverloadedStrings]]
                loadModules [dir ++ "/" ++ name ++ ".hs"]
                setTopLevelModules [name]
                setImportsF [ ModuleImport "Prelude" NotQualified NoImportList
                            , ModuleImport "Game" NotQualified NoImportList
                            , ModuleImport "Contexts" NotQualified NoImportList
                            , ModuleImport "Parse" NotQualified NoImportList
                            , ModuleImport "Evaluate" NotQualified NoImportList
                            , ModuleImport "Data.Tree" NotQualified NoImportList
                            , ModuleImport "Data.Text" NotQualified (ImportList ["Text", "pack", "unpack", "append"])
                            , ModuleImport "Data.Text" (QualifiedAs $ Just "T") NoImportList
                            , ModuleImport "Data.Text.Lazy" (QualifiedAs $ Just "TL") NoImportList
                            , ModuleImport "Data.Map" NotQualified (ImportList ["Map"])
                            , ModuleImport "Data.Map" (QualifiedAs $ Just "M") NoImportList
                            , ModuleImport "Formatting" NotQualified NoImportList
                            , ModuleImport "Formatting.Formatters" NotQualified NoImportList
                            ]
                
                interpret ("printer") (as :: Printer)

data ModuleDatum =
    ModuleDatum { namedatum :: Text
                , mixdatum :: MixupData
                , scoreattdatum :: Score
                , scoredefdatum :: Score
                , enddatum :: EndState
                , updatum :: Updater
                , printdatum :: Printer
                , printpath :: Text
                , outdatum :: Recontext
    }
data ModuleData =
    ModuleData { mixdata :: Map Text MixupData
               , scoredata :: Map Text Score
               , enddata :: Map Text EndState
               , updata :: Map Text Updater
               , printdata :: Map Text Printer
               , instrdata :: [Instruction]
    }
type ModuleReader = ReaderT ModuleDatum FlagReader
type FlagReader = ReaderT Flags IO

environment :: FlagReader ModuleData
environment = do
    resetLog
    args <- ask
    instructions <- liftIO . readInstructions . flagConfig $ args
    if null instructions then error ("No instructions found in " ++ (flagConfig args) ++ "!") else logger ("Found instructions in " ++ (flagConfig args) ++ ":")
    mapM_ logger . map (\instr -> "- " ++ (unpack . Parse.name $ instr)) $ instructions
    
    let instrScoreData = nub . map scores $ instructions
    let instrScores = nub . concat $ (map (map $ unpack . scoreNameAtt) instrScoreData ++ map (map $ unpack . scoreNameDef) instrScoreData)
    let instrEnds = nub . concat . map (map $ unpack . endName) $ instrScoreData
    let instrUpdaters = nub . concat . map (map $ unpack . updateName) $ instrScoreData
    let instrPrinters = nub . concat . map (map $ unpack . outType) $ instrScoreData
    let instrData = nub . (map $ unpack . path) $ instructions
    logger "Read instructions!"
    
    foundScores     <- liftIO $ findFiles . flagScores $ args
    foundEnds       <- liftIO $ findFiles . flagEndStates $ args
    foundUpdaters   <- liftIO $ findFiles . flagUpdaters $ args
    foundPrinters   <- liftIO $ findFiles . flagPrinters $ args
    foundData       <- liftIO $ findFiles . flagIn $ args
    
    let reqScores = filter (`elem` instrScores) . nub $ foundScores
    let reqEnds = filter (`elem` instrEnds) . nub $ foundEnds
    let reqUpdaters = filter (`elem` instrUpdaters) . nub $ foundUpdaters
    let reqPrinters = filter (`elem` instrPrinters) . nub $ foundPrinters
    let reqData = filter (`elem` instrData) . nub $ foundData
    
    unless (null $ instrScores \\ reqScores) $ error $ "Required Score modules missing in /" ++ (flagScores args) ++ "! Missing modules: " ++ (foldl (\acc file -> acc ++ "\n- " ++ file) "" $ instrScores \\ reqScores)
    logger "Required Score modules:"
    mapM_ logger . map ("- "++) $  reqScores
    
    unless (null $ instrEnds \\ reqEnds) $ error $ "Required EndState modules missing in /" ++ (flagEndStates args) ++ "! Missing modules: " ++ (foldl (\acc file -> acc ++ "\n- " ++ file) "" $ instrEnds \\ reqEnds)
    logger "Required EndState modules:"
    mapM_ logger . map ("- "++) $  reqEnds
    
    unless (null $ instrUpdaters \\ reqUpdaters) $ error $ "Required Updater modules missing in /" ++ (flagUpdaters args) ++ "! Missing modules: " ++ (foldl (\acc file -> acc ++ "\n- " ++ file) "" $ instrUpdaters \\ reqUpdaters)
    logger "Required Updater modules:"
    mapM_ logger . map ("- "++) $  reqUpdaters
    
    unless (null $ instrPrinters \\ reqPrinters) $ error $ "Required Printer modules missing in /" ++ (flagPrinters args) ++ "! Missing modules: " ++ (foldl (\acc file -> acc ++ "\n- " ++ file) "" $ instrPrinters \\ reqPrinters)
    logger "Required Printer modules:"
    mapM_ logger . map ("- "++) $  reqPrinters
    
    unless (null $ instrData \\ reqData) $ error $ "Required input data missing in /" ++ (flagIn args) ++ "! Missing files: " ++ (foldl (\acc file -> acc ++ "\n- " ++ file) "" $ instrData \\ reqData)
    logger "Required input data:"
    mapM_ logger . map ("- "++) $ reqData
    
    parsedScores <- liftIO $ mapM (parseScript parseScore (flagScores args)) reqScores
    logger "Parsed all required Score modules!"
    parsedEnds <- liftIO $ mapM (parseScript parseEndState (flagEndStates args)) reqEnds
    logger "Parsed all required EndState modules!"
    parsedUpdaters <- liftIO $ mapM (parseScript parseUpdater (flagUpdaters args)) reqUpdaters
    logger "Parsed all required Updater modules!"
    parsedPrinters <- liftIO $ mapM (parseScript parsePrinter (flagPrinters args)) reqPrinters
    logger "Parsed all required Printer modules!"
    parsedData <- liftIO $ mapM (\file -> parseData $ flagIn args ++ "/" ++ file) reqData
    logger "Parsed all required input data!"
    
    let scoreLookup = Map.fromList . zip (map pack reqScores) $! parsedScores
    let endLookup = Map.fromList . zip (map pack reqEnds) $! parsedEnds
    let updaterLookup = Map.fromList . zip (map pack reqUpdaters) $! parsedUpdaters
    let printerLookup = Map.fromList . zip (map pack reqPrinters) $! parsedPrinters
    let dataLookup = Map.fromList . zip (map pack reqData) $! parsedData
    logger "Created lookup tables!"
    
    return $ ModuleData dataLookup scoreLookup endLookup updaterLookup printerLookup instructions
    where
        findFiles :: String -> IO ([String])
        findFiles = fmap (map $ concat . init . splitOn ".") . listDirectory . makeValid
        
        parseScript :: Show a => (FilePath -> String -> IO (Either a b)) -> FilePath -> String -> IO b
        parseScript f dir file = fmap (either (\e -> error . show $ e) id) . f dir $ file

getModule :: (Monad b) => String -> (ModuleData -> Map Text a) -> ReaderT ModuleData b (Text -> a)
getModule err f = do
    env <- ask
    return (\t -> fromMaybe (error $ err ++ ": " ++ (unpack t)) . Map.lookup t . f $ env)

resetLog :: FlagReader ()
resetLog = do
    f <- fmap flagLog ask
    liftIO $ writeFile f ""
logger :: String -> FlagReader ()
logger s = do
    v <- fmap flagVerbose ask
    f <- fmap flagLog ask
    when v . liftIO . putStrLn $ s
    liftIO . appendFile f . (++"\n") $ s
writer :: FilePath -> String -> FlagReader ()
writer path s = do
    outdir <- fmap flagOut ask
    liftIO $ createDirectoryIfMissing True outdir
    liftIO $ writeFile (outdir ++ "/" ++ path) s
