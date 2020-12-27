module Score
    ( Score
    , EndState
    , Updater
    , Printer
    , ModuleData
    , Modules
    , ModulesT
    , environment
    , getData
    , getScore
    , getEnd
    , getUpdater
    , getPrinter
    , ModuleDatum (ModuleDatum, mixdatum, scoredatum, enddatum, updatum, printdatum, printpath, outdatum)
    ) where

import Contexts
import Parse

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
    }
type Modules = Reader ModuleData
type ModulesT = ReaderT ModuleData

environment :: [Instruction] -> IO ModuleData
environment instructions = do
    if null instructions then error ("No instructions found in config!") else putStrLn "Found instructions in config.yaml:"
    mapM_ putStrLn . map (\instr -> "- "++(unpack . Parse.name $ instr)) $ instructions
    
    let instrScoreData = nub . map scores $ instructions
    let instrScores = nub . concat . map (map $ unpack . scoreName) $ instrScoreData
    let instrEnds = nub . concat . map (map $ unpack . endName) $ instrScoreData
    let instrUpdaters = nub . concat . map (map $ unpack . updateName) $ instrScoreData
    let instrPrinters = nub . concat . map (map $ unpack . outType) $ instrScoreData
    let instrData = nub . (map $ unpack . path) $ instructions
    putStrLn "Read instructions!"
    
    foundScores     <- findFiles "score"
    foundEnds       <- findFiles "endstate"
    foundUpdaters   <- findFiles "updater"
    foundPrinters   <- findFiles "printer"
    foundData       <- findFiles "in"
    
    let reqScores = filter (`elem` instrScores) . nub $ foundScores
    let reqEnds = filter (`elem` instrEnds) . nub $ foundEnds
    let reqUpdaters = filter (`elem` instrUpdaters) . nub $ foundUpdaters
    let reqPrinters = filter (`elem` instrPrinters) . nub $ foundPrinters
    let reqData = filter (`elem` instrData) . nub $ foundData
    
    unless (null $ instrScores \\ reqScores) $ error $ "Required Score modules missing in /score! Missing modules: " ++ (foldl (\acc file -> acc ++ "\n- " ++ file) "" $ instrScores \\ reqScores)
    putStrLn "Required Score modules:"
    mapM_ putStrLn . map ("- "++) $  reqScores
    
    unless (null $ instrEnds \\ reqEnds) $ error $ "Required EndState modules missing in /endstate! Missing modules: " ++ (foldl (\acc file -> acc ++ "\n- " ++ file) "" $ instrEnds \\ reqEnds)
    putStrLn "Required EndState modules:"
    mapM_ putStrLn . map ("- "++) $  reqEnds
    
    unless (null $ instrUpdaters \\ reqUpdaters) $ error $ "Required Updater modules missing in /updater! Missing modules: " ++ (foldl (\acc file -> acc ++ "\n- " ++ file) "" $ instrUpdaters \\ reqUpdaters)
    putStrLn "Required Updater modules:"
    mapM_ putStrLn . map ("- "++) $  reqUpdaters
    
    unless (null $ instrPrinters \\ reqPrinters) $ error $ "Required Printer modules missing in /printer! Missing modules: " ++ (foldl (\acc file -> acc ++ "\n- " ++ file) "" $ instrPrinters \\ reqPrinters)
    putStrLn "Required Printer modules:"
    mapM_ putStrLn . map ("- "++) $  reqPrinters
    
    unless (null $ instrData \\ reqData) $ error $ "Required input data missing in /in! Missing files: " ++ (foldl (\acc file -> acc ++ "\n- " ++ file) "" $ instrData \\ reqData)
    putStrLn "Required input data:"
    mapM_ putStrLn . map ("- "++) $ reqData
    
    parsedScores <- mapM (parseScript score) reqScores
    putStrLn "Parsed all required Score modules!"
    parsedEnds <- mapM (parseScript end) reqEnds
    putStrLn "Parsed all required EndState modules!"
    parsedUpdaters <- mapM (parseScript update) reqUpdaters
    putStrLn "Parsed all required Updater modules!"
    parsedPrinters <- mapM (parseScript printer) reqPrinters
    putStrLn "Parsed all required Printer modules!"
    parsedData <- mapM parseData reqData
    putStrLn "Parsed all required input data!"
    
    let scoreLookup = Map.fromList . zip (map pack reqScores) $! parsedScores
    let endLookup = Map.fromList . zip (map pack reqEnds) $! parsedEnds
    let updaterLookup = Map.fromList . zip (map pack reqUpdaters) $! parsedUpdaters
    let printerLookup = Map.fromList . zip (map pack reqPrinters) $! parsedPrinters
    let dataLookup = Map.fromList . zip (map pack reqData) $! parsedData
    putStrLn "Created lookup tables!"
    
    return $ ModuleData dataLookup scoreLookup endLookup updaterLookup printerLookup
    where
        findFiles :: String -> IO ([String])
        findFiles = fmap (map $ concat . init . splitOn ".") . listDirectory . makeValid
        
        parseScript :: Show a => (String -> IO (Either a b)) -> String -> IO b
        parseScript f = fmap (either (\e -> error . show $ e) id) . f

getModule :: (Monad b) => String -> (ModuleData -> Map Text a) -> ModulesT b (Text -> a)
getModule err f = do
    env <- ask
    return (\t -> fromMaybe (error $ err ++ ": " ++ (unpack t)) . Map.lookup t . f $ env)
getData :: (Monad a) => ModulesT a (Text -> MixupData)
getData = getModule "Tried to get nonexistent input data" mixdata
getScore :: (Monad a) => ModulesT a (Text -> Score)
getScore = getModule "Tried to get a nonexistent score" scoredata
getEnd :: (Monad a) => ModulesT a (Text -> EndState)
getEnd = getModule "Tried to get a nonexistent endstate" enddata
getUpdater :: (Monad a) => ModulesT a (Text -> Updater)
getUpdater = getModule "Tried to get a nonexistent updater" updata
getPrinter :: (Monad a) => ModulesT a (Text -> Printer)
getPrinter = getModule "Tried to get a nonexistent printer" printdata
