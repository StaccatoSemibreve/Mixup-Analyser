module Score
    ( Score
    , EndState
    , Updater
    , Printer
    , ModuleDatum (ModuleDatum, mixdatum, scoredatum, enddatum, updatum, printdatum, printpath, outdatum)
    , ModuleData (ModuleData, mixdata, scoredata, enddata, updata, printdata, instrdata)
    , ModuleReader
    , environment
    , getModule
    ) where

import Contexts
import Parse
import Args

import Data.Maybe
import Control.Monad
import Data.List
import Data.List.Split
import Data.Text (Text, pack, unpack)
import System.Directory (listDirectory)
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

score :: String -> IO (Either InterpreterError Score)
score name = runInterpreter $ do
                I.set [languageExtensions := [OverloadedStrings]]
                loadModules ["score/" ++ name ++ ".hs"]
                setTopLevelModules [name]
                setImportsF [ ModuleImport "Prelude" NotQualified NoImportList
                            , ModuleImport "Contexts" NotQualified NoImportList
                            , ModuleImport "Data.Text" NotQualified (ImportList ["Text"])
                            , ModuleImport "Data.Map" NotQualified (ImportList ["Map"])
                            ]
                
                interpret ("score") (as :: Score)

end :: String -> IO (Either InterpreterError EndState)
end name = runInterpreter $ do
                I.set [languageExtensions := [OverloadedStrings]]
                loadModules ["endstate/" ++ name ++ ".hs"]
                setTopLevelModules [name]
                setImportsF [ ModuleImport "Prelude" NotQualified NoImportList
                            , ModuleImport "Contexts" NotQualified NoImportList
                            , ModuleImport "Data.Text" NotQualified (ImportList ["Text"])
                            , ModuleImport "Data.Map" NotQualified (ImportList ["Map"])
                            ]
                
                interpret ("end") (as :: EndState)

update :: String -> IO (Either InterpreterError Updater)
update name = runInterpreter $ do
                I.set [languageExtensions := [OverloadedStrings]]
                loadModules ["updater/" ++ name ++ ".hs"]
                setTopLevelModules [name]
                setImportsF [ ModuleImport "Prelude" NotQualified NoImportList
                            , ModuleImport "Contexts" NotQualified NoImportList
                            , ModuleImport "Data.Text" NotQualified (ImportList ["Text"])
                            , ModuleImport "Data.Map" NotQualified (ImportList ["Map"])
                            ]
                
                interpret ("update") (as :: Updater)

printer :: String -> IO (Either InterpreterError Printer)
printer name = runInterpreter $ do
                I.set [languageExtensions := [OverloadedStrings]]
                loadModules ["printer/" ++ name ++ ".hs"]
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
    ModuleDatum { mixdatum :: MixupData
                , scoredatum :: Score
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
type ModuleReader = ReaderT ModuleDatum IO

environment :: ReaderT Flags IO ModuleData
environment = do
    args <- ask
    instructions <- liftIO . readInstructions . flagConfig $ args
    liftIO $ if null instructions then error ("No instructions found in " ++ (flagConfig args) ++ "!") else putStrLn ("Found instructions in " ++ (flagConfig args) ++ ":")
    liftIO . mapM_ putStrLn . map (\instr -> "- " ++ (unpack . Parse.name $ instr)) $ instructions
    
    let instrScoreData = nub . map scores $ instructions
    let instrScores = nub . concat . map (map $ unpack . scoreName) $ instrScoreData
    let instrEnds = nub . concat . map (map $ unpack . endName) $ instrScoreData
    let instrUpdaters = nub . concat . map (map $ unpack . updateName) $ instrScoreData
    let instrPrinters = nub . concat . map (map $ unpack . outType) $ instrScoreData
    let instrData = nub . (map $ unpack . path) $ instructions
    liftIO . putStrLn $ "Read instructions!"
    
    foundScores     <- liftIO $ findFiles "score"
    foundEnds       <- liftIO $ findFiles "endstate"
    foundUpdaters   <- liftIO $ findFiles "updater"
    foundPrinters   <- liftIO $ findFiles "printer"
    foundData       <- liftIO $ findFiles "in"
    
    let reqScores = filter (`elem` instrScores) . nub $ foundScores
    let reqEnds = filter (`elem` instrEnds) . nub $ foundEnds
    let reqUpdaters = filter (`elem` instrUpdaters) . nub $ foundUpdaters
    let reqPrinters = filter (`elem` instrPrinters) . nub $ foundPrinters
    let reqData = filter (`elem` instrData) . nub $ foundData
    
    unless (null $ instrScores \\ reqScores) $ error $ "Required Score modules missing in /score! Missing modules: " ++ (foldl (\acc file -> acc ++ "\n- " ++ file) "" $ instrScores \\ reqScores)
    liftIO $ putStrLn "Required Score modules:"
    liftIO . mapM_ putStrLn . map ("- "++) $  reqScores
    
    unless (null $ instrEnds \\ reqEnds) $ error $ "Required EndState modules missing in /endstate! Missing modules: " ++ (foldl (\acc file -> acc ++ "\n- " ++ file) "" $ instrEnds \\ reqEnds)
    liftIO $ putStrLn "Required EndState modules:"
    liftIO . mapM_ putStrLn . map ("- "++) $  reqEnds
    
    unless (null $ instrUpdaters \\ reqUpdaters) $ error $ "Required Updater modules missing in /updater! Missing modules: " ++ (foldl (\acc file -> acc ++ "\n- " ++ file) "" $ instrUpdaters \\ reqUpdaters)
    liftIO $ putStrLn "Required Updater modules:"
    liftIO . mapM_ putStrLn . map ("- "++) $  reqUpdaters
    
    unless (null $ instrPrinters \\ reqPrinters) $ error $ "Required Printer modules missing in /printer! Missing modules: " ++ (foldl (\acc file -> acc ++ "\n- " ++ file) "" $ instrPrinters \\ reqPrinters)
    liftIO $ putStrLn "Required Printer modules:"
    liftIO . mapM_ putStrLn . map ("- "++) $  reqPrinters
    
    unless (null $ instrData \\ reqData) $ error $ "Required input data missing in /in! Missing files: " ++ (foldl (\acc file -> acc ++ "\n- " ++ file) "" $ instrData \\ reqData)
    liftIO $ putStrLn "Required input data:"
    liftIO . mapM_ putStrLn . map ("- "++) $ reqData
    
    parsedScores <- liftIO $ mapM (parseScript score) reqScores
    liftIO $ putStrLn "Parsed all required Score modules!"
    parsedEnds <- liftIO $ mapM (parseScript end) reqEnds
    liftIO $ putStrLn "Parsed all required EndState modules!"
    parsedUpdaters <- liftIO $ mapM (parseScript update) reqUpdaters
    liftIO $ putStrLn "Parsed all required Updater modules!"
    parsedPrinters <- liftIO $ mapM (parseScript printer) reqPrinters
    liftIO $ putStrLn "Parsed all required Printer modules!"
    parsedData <- liftIO $ mapM parseData reqData
    liftIO $ putStrLn "Parsed all required input data!"
    
    let scoreLookup = Map.fromList . zip (map pack reqScores) $! parsedScores
    let endLookup = Map.fromList . zip (map pack reqEnds) $! parsedEnds
    let updaterLookup = Map.fromList . zip (map pack reqUpdaters) $! parsedUpdaters
    let printerLookup = Map.fromList . zip (map pack reqPrinters) $! parsedPrinters
    let dataLookup = Map.fromList . zip (map pack reqData) $! parsedData
    liftIO $ putStrLn "Created lookup tables!"
    
    return $ ModuleData dataLookup scoreLookup endLookup updaterLookup printerLookup instructions
    where
        findFiles :: String -> IO ([String])
        findFiles = fmap (map $ concat . init . splitOn ".") . listDirectory . makeValid
        
        parseScript :: Show a => (String -> IO (Either a b)) -> String -> IO b
        parseScript f = fmap (either (\e -> error . show $ e) id) . f

getModule :: (Monad b) => String -> (ModuleData -> Map Text a) -> ReaderT ModuleData b (Text -> a)
getModule err f = do
    env <- ask
    return (\t -> fromMaybe (error $ err ++ ": " ++ (unpack t)) . Map.lookup t . f $ env)
