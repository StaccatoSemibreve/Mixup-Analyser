module Score
    ( askMods, askFlags
    , environment, getModule
    , loggerF, writerF
    , logger, writer
    , scoreatt, scoredef, scoresatt, scoresdef, updateContext, endCheck, printTree
    ) where

import ScoreData
import Contexts
import ParseData
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
import Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as HashMap
import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.State.Lazy (evalState, execState)

parseScore :: FilePath -> String -> IO (Either InterpreterError Score)
parseScore dir name = runInterpreter $ do
                I.set [languageExtensions := [OverloadedStrings]]
                loadModules [dir ++ "/" ++ name ++ ".hs"]
                setTopLevelModules [name]
                setImportsF [ ModuleImport "Prelude" NotQualified NoImportList
                            , ModuleImport "Contexts" NotQualified NoImportList
                            , ModuleImport "Score" NotQualified NoImportList
                            , ModuleImport "Data.Text" NotQualified NoImportList
                            , ModuleImport "Data.HashMap.Lazy" NotQualified NoImportList
                            , ModuleImport "Control.Monad.State.Lazy" NotQualified NoImportList
                            , ModuleImport "Data.Functor.Identity" NotQualified NoImportList
                            , ModuleImport "Control.Monad" NotQualified NoImportList
                            ]
                
                interpret ("score") (as :: Score)

parseEndState :: FilePath -> String -> IO (Either InterpreterError EndState)
parseEndState dir name = runInterpreter $ do
                I.set [languageExtensions := [OverloadedStrings]]
                loadModules [dir ++ "/" ++ name ++ ".hs"]
                setTopLevelModules [name]
                setImportsF [ ModuleImport "Prelude" NotQualified NoImportList
                            , ModuleImport "Contexts" NotQualified NoImportList
                            , ModuleImport "Data.Text" NotQualified NoImportList
                            , ModuleImport "Data.HashMap.Lazy" NotQualified NoImportList
                            , ModuleImport "Control.Monad.State.Lazy" NotQualified NoImportList
                            , ModuleImport "Data.Functor.Identity" NotQualified NoImportList
                            , ModuleImport "Control.Monad" NotQualified NoImportList
                            ]
                
                interpret ("end") (as :: EndState)

parseUpdater :: FilePath -> String -> IO (Either InterpreterError Updater)
parseUpdater dir name = runInterpreter $ do
                I.set [languageExtensions := [OverloadedStrings]]
                loadModules [dir ++ "/" ++ name ++ ".hs"]
                setTopLevelModules [name]
                setImportsF [ ModuleImport "Prelude" NotQualified NoImportList
                            , ModuleImport "Contexts" NotQualified NoImportList
                            , ModuleImport "Data.Text" NotQualified NoImportList
                            , ModuleImport "Data.HashMap.Lazy" NotQualified NoImportList
                            , ModuleImport "Control.Monad.State.Lazy" NotQualified NoImportList
                            , ModuleImport "Data.Functor.Identity" NotQualified NoImportList
                            , ModuleImport "Control.Monad" NotQualified NoImportList
                            ]
                
                interpret ("updater") (as :: Updater)

parsePrinter :: FilePath -> String -> IO (Either InterpreterError Printer)
parsePrinter dir name = runInterpreter $ do
                I.set [languageExtensions := [OverloadedStrings]]
                loadModules [dir ++ "/" ++ name ++ ".hs"]
                setTopLevelModules [name]
                setImportsF [ ModuleImport "Prelude" NotQualified NoImportList
                            , ModuleImport "Game" NotQualified NoImportList
                            , ModuleImport "Contexts" NotQualified NoImportList
                            , ModuleImport "ScoreData" NotQualified NoImportList
                            , ModuleImport "ParseData" NotQualified NoImportList
                            , ModuleImport "Evaluate" NotQualified NoImportList
                            , ModuleImport "Data.Tree" NotQualified NoImportList
                            , ModuleImport "Data.Text" NotQualified NoImportList
                            , ModuleImport "Data.Text" (QualifiedAs $ Just "T") NoImportList
                            , ModuleImport "Data.Text.Lazy" (QualifiedAs $ Just "TL") NoImportList
                            , ModuleImport "Data.Text.Encoding" (QualifiedAs $ Just "TE") NoImportList
                            , ModuleImport "Data.Text.Lazy.Encoding" (QualifiedAs $ Just "TLE") NoImportList
                            , ModuleImport "Data.HashMap.Lazy" NotQualified NoImportList
                            , ModuleImport "Data.Map" (QualifiedAs $ Just "M") NoImportList
                            , ModuleImport "Formatting" NotQualified NoImportList
                            , ModuleImport "Formatting.Formatters" NotQualified NoImportList
                            , ModuleImport "Formatting.Combinators" NotQualified NoImportList
                            , ModuleImport "Control.Monad" NotQualified NoImportList
                            , ModuleImport "Control.Monad.State.Lazy" NotQualified NoImportList
                            , ModuleImport "Control.Monad.Reader" NotQualified NoImportList
                            , ModuleImport "Data.YAML" NotQualified NoImportList
                            ]
                
                interpret ("printer") (as :: Printer)

askMods :: (Monad a) => ModuleReader a ModuleDatum
askMods = fmap fst ask
askFlags :: (Monad a) => ModuleReader a Flags
askFlags = fmap snd ask

environment :: FlagReader ModuleData
environment = do
    resetLogF
    args <- ask
    instructions <- liftIO . readInstructions . flagConfig $ args
    if null instructions then error ("No instructions found in " ++ (flagConfig args) ++ "!") else loggerF ("Found instructions in " ++ (flagConfig args) ++ ":")
    mapM_ loggerF . map (\instr -> "- " ++ (unpack . ParseData.name $ instr)) $ instructions
    
    let instrScoreData = nub . map scores $ instructions
    let instrScores = nub . concat . concat $ (map (map $ map unpack . scoreNamesAtt) instrScoreData ++ map (map $ map unpack . scoreNamesDef) instrScoreData)
    let instrEnds = nub . concat . map (map $ unpack . endName) $ instrScoreData
    let instrUpdaters = nub . concat . map (map $ unpack . updateName) $ instrScoreData
    let instrPrinters = nub . concat . map (map $ unpack . outType) $ instrScoreData
    let instrData = nub . (map $ unpack . path) $ instructions
    loggerF "Read instructions!"
    
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
    loggerF "Required Score modules:"
    mapM_ loggerF . map ("- "++) $  reqScores
    
    unless (null $ instrEnds \\ reqEnds) $ error $ "Required EndState modules missing in /" ++ (flagEndStates args) ++ "! Missing modules: " ++ (foldl (\acc file -> acc ++ "\n- " ++ file) "" $ instrEnds \\ reqEnds)
    loggerF "Required EndState modules:"
    mapM_ loggerF . map ("- "++) $  reqEnds
    
    unless (null $ instrUpdaters \\ reqUpdaters) $ error $ "Required Updater modules missing in /" ++ (flagUpdaters args) ++ "! Missing modules: " ++ (foldl (\acc file -> acc ++ "\n- " ++ file) "" $ instrUpdaters \\ reqUpdaters)
    loggerF "Required Updater modules:"
    mapM_ loggerF . map ("- "++) $  reqUpdaters
    
    unless (null $ instrPrinters \\ reqPrinters) $ error $ "Required Printer modules missing in /" ++ (flagPrinters args) ++ "! Missing modules: " ++ (foldl (\acc file -> acc ++ "\n- " ++ file) "" $ instrPrinters \\ reqPrinters)
    loggerF "Required Printer modules:"
    mapM_ loggerF . map ("- "++) $  reqPrinters
    
    unless (null $ instrData \\ reqData) $ error $ "Required input data missing in /" ++ (flagIn args) ++ "! Missing files: " ++ (foldl (\acc file -> acc ++ "\n- " ++ file) "" $ instrData \\ reqData)
    loggerF "Required input data:"
    mapM_ loggerF . map ("- "++) $ reqData
    
    parsedScores <- liftIO $ mapM (parseScript parseScore (flagScores args)) reqScores
    loggerF "Parsed all required Score modules!"
    parsedEnds <- liftIO $ mapM (parseScript parseEndState (flagEndStates args)) reqEnds
    loggerF "Parsed all required EndState modules!"
    parsedUpdaters <- liftIO $ mapM (parseScript parseUpdater (flagUpdaters args)) reqUpdaters
    loggerF "Parsed all required Updater modules!"
    parsedPrinters <- liftIO $ mapM (parseScript parsePrinter (flagPrinters args)) reqPrinters
    loggerF "Parsed all required Printer modules!"
    parsedData <- liftIO $ mapM (\file -> parseData $ flagIn args ++ "/" ++ file) reqData
    loggerF "Parsed all required input data!"
    
    let scoreLookup = HashMap.fromList . zip (map pack reqScores) $! parsedScores
    let endLookup = HashMap.fromList . zip (map pack reqEnds) $! parsedEnds
    let updaterLookup = HashMap.fromList . zip (map pack reqUpdaters) $! parsedUpdaters
    let printerLookup = HashMap.fromList . zip (map pack reqPrinters) $! parsedPrinters
    let dataLookup = HashMap.fromList . zip (map pack reqData) $! parsedData
    loggerF "Created lookup tables!"
    
    return $ ModuleData dataLookup scoreLookup endLookup updaterLookup printerLookup instructions
    where
        findFiles :: String -> IO ([String])
        findFiles = fmap (map $ concat . init . splitOn ".") . listDirectory . makeValid
        
        parseScript :: Show a => (FilePath -> String -> IO (Either a b)) -> FilePath -> String -> IO b
        parseScript f dir file = fmap (either (\e -> error . show $ e) id) . f dir $ file

getModule :: (Monad b) => String -> (ModuleData -> HashMap Text a) -> ReaderT ModuleData b (Text -> a)
getModule err f = do
    env <- ask
    return (\t -> fromMaybe (error $ err ++ ": " ++ (unpack t)) . HashMap.lookup t . f $ env)

scoreatt :: (Monad a) => Context -> ModuleReader a Double
scoreatt c = do
    f <- fmap (snd . head . scoreattdatum) askMods
    return $ evalState f c
scoredef :: (Monad a) => Context -> ModuleReader a Double
scoredef c = do
    f <- fmap (snd . head . scoredefdatum) askMods
    return $ evalState f c

scoresatt :: (Monad a) => Context -> ModuleReader a [Double]
scoresatt c = do
    fs <- fmap (map snd . scoreattdatum) askMods
    return $ map (\f -> evalState f c) fs
scoresdef :: (Monad a) => Context -> ModuleReader a [Double]
scoresdef c = do
    fs <- fmap (map snd . scoredefdatum) askMods
    return $ map (\f -> evalState f c) fs

endCheck :: (Monad a) => Context -> ModuleReader a (Bool)
endCheck c = do
    f <- fmap (snd .enddatum) askMods
    return $ evalState f c

updateContext :: (Monad a) => Context -> ModuleReader a (Context)
updateContext c = do
    f <- fmap (snd . updatum) askMods
    return $ execState f c

printTree :: (Monad a) => TreeGame -> ModuleReader a Text
printTree tree = do
    f <- fmap (snd . printdatum) askMods
    mapReaderT (return . runIdentity) (f tree)

resetLogF :: FlagReader ()
resetLogF = do
    f <- fmap flagLog ask
    liftIO $ writeFile f ""
loggerF :: String -> FlagReader ()
loggerF s = do
    v <- fmap flagVerbose ask
    f <- fmap flagLog ask
    when v . liftIO . putStrLn $ s
    liftIO . appendFile f . (++"\n") $ s
writerF :: FilePath -> String -> FlagReader ()
writerF path s = do
    outdir <- fmap flagOut ask
    liftIO $ createDirectoryIfMissing True outdir
    liftIO $ writeFile (outdir ++ "/" ++ path) s

resetLog :: ModuleReader IO ()
resetLog = do
    f <- fmap flagLog askFlags
    liftIO $ writeFile f ""
logger :: String -> ModuleReader IO ()
logger s = do
    v <- fmap flagVerbose askFlags
    f <- fmap flagLog askFlags
    when v . liftIO . putStrLn $ s
    liftIO . appendFile f . (++"\n") $ s
writer :: FilePath -> String -> ModuleReader IO ()
writer path s = do
    outdir <- fmap flagOut askFlags
    liftIO $ createDirectoryIfMissing True outdir
    liftIO $ writeFile (outdir ++ "/" ++ path) s
