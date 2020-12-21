module Score
    ( score
    , end
    , update
    , printer
    ) where

import Game
import Contexts
import Parse
import Evaluate

import Control.Monad
import Language.Haskell.Interpreter
import qualified Language.Haskell.Interpreter as I

score :: String -> IO (Either InterpreterError (Context -> Double))
score name = runInterpreter $ do
                I.set [languageExtensions := [OverloadedStrings]]
                loadModules ["score/" ++ name ++ ".hs"]
                setTopLevelModules [name]
                setImportsF [ ModuleImport "Prelude" NotQualified NoImportList
                            , ModuleImport "Contexts" NotQualified NoImportList
                            , ModuleImport "Data.Text" NotQualified (ImportList ["Text"])
                            , ModuleImport "Data.Map" NotQualified (ImportList ["Map"])
                            ]
                
                interpret ("score") (as :: Context -> Double)

end :: String -> IO (Either InterpreterError (Context -> Bool))
end name = runInterpreter $ do
                I.set [languageExtensions := [OverloadedStrings]]
                loadModules ["endstate/" ++ name ++ ".hs"]
                setTopLevelModules [name]
                setImportsF [ ModuleImport "Prelude" NotQualified NoImportList
                            , ModuleImport "Contexts" NotQualified NoImportList
                            , ModuleImport "Data.Text" NotQualified (ImportList ["Text"])
                            , ModuleImport "Data.Map" NotQualified (ImportList ["Map"])
                            ]
                
                interpret ("end") (as :: Context -> Bool)

update :: String -> IO (Either InterpreterError (Context -> Context))
update name = runInterpreter $ do
                I.set [languageExtensions := [OverloadedStrings]]
                loadModules ["updater/" ++ name ++ ".hs"]
                setTopLevelModules [name]
                setImportsF [ ModuleImport "Prelude" NotQualified NoImportList
                            , ModuleImport "Contexts" NotQualified NoImportList
                            , ModuleImport "Data.Text" NotQualified (ImportList ["Text"])
                            , ModuleImport "Data.Map" NotQualified (ImportList ["Map"])
                            ]
                
                interpret ("update") (as :: Context -> Context)

printer :: String -> IO (Either InterpreterError (TreeGame -> String))
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
                            , ModuleImport "Data.Map" NotQualified (ImportList ["Map"])
                            , ModuleImport "Data.Map" (QualifiedAs $ Just "M") NoImportList
                            ]
                
                interpret ("printer") (as :: TreeGame -> String)
