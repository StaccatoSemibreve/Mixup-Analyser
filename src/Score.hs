module Score
    ( score
    , Score
    , end
    , EndState
    , update
    , Updater
    , printer
    , Printer
    ) where

import Contexts
import Evaluate

import Control.Monad
import Data.Text (Text)
import Language.Haskell.Interpreter
import qualified Language.Haskell.Interpreter as I
import Formatting
import Formatting.Formatters

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
