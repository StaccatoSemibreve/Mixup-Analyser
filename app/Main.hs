{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

-- calculates nash equilibriums, does related backend stuff
import Game
-- parses yaml into specific types also stored here
import Parse
-- manipulates the yaml data into the relevant data trees, evaluates with a fold (well, a scan) using that
import Evaluate
-- the helper functions used by Custom so that it's not also full of things that should never be altered
import Contexts
-- handles the score, endstate check, and context updating functions, defined at runtime
import Score
-- handles the program arguments
import Args

import Data.Maybe
import Data.List
import Data.List.Split
import Data.Text (Text, unpack, pack)
import Data.Tree
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad
import Control.Comonad
import System.Environment
import System.Directory (listDirectory)
import System.FilePath (makeValid)
import Control.Monad.Memo (MonadMemo, startEvalMemo)
import qualified Control.Monad.Memo as Memo
import Criterion.Main
import Control.Monad.Reader

main :: IO ()
main = do
    args <- getArgs >>= opts
    putStrLn . show $ args
    program args

program :: ([Flag], [String]) -> IO ()
program args = do
    let (Config config) = Config "config.yaml"
    instructions <- readInstructions config
    if null instructions then error ("No instructions found in " ++ config ++ "!") else putStrLn "Found instructions in config.yaml:"
    mapM_ putStrLn . map (\instr -> "- "++(unpack . name $ instr)) $ instructions
    
    env <- environment instructions
    
    let roots = (flip runReader) env . plantTrees $ instructions
    putStrLn "Planted context trees!"
    let contexttrees = (flip runReader) env . sequence . map growTree $ roots
    putStrLn "Grown context trees, using their respective EndState and Updater modules!"
    let gametrees = startEvalMemo . (flip runReaderT) env . sequence . map gamifyTree $ contexttrees
    putStrLn $ "Analysed context trees, using their respective Score modules! The next step may take a moment to begin."
    
    mapM_ ((flip runReaderT) env . exportTree) gametrees
    putStrLn "Done!"
    where
        findFiles :: String -> IO ([String])
        findFiles = fmap (map $ concat . init . splitOn ".") . listDirectory . makeValid
        
        plantTrees :: [Instruction] -> Modules [ModuleDatum]
        plantTrees instrs = do
            unflatTrees <- mapM plantTree instrs
            return . concat $ unflatTrees
            where
                plantTree :: Instruction -> Modules [ModuleDatum]
                plantTree instr = sequence . map (seed instr) . scores $ instr
                
                seed :: Instruction -> ScoreData -> Modules ModuleDatum
                seed instr sdata = do
                    mdata <- fmap ($ path instr) getData
                    fS <- fmap ($ scoreName sdata) getScore
                    fE <- fmap ($ endName sdata) getEnd
                    fU <- fmap ($ updateName sdata) getUpdater
                    fP <- fmap ($ outType sdata) getPrinter
                    return $ ModuleDatum mdata fS fE fU fP (outPath sdata) (context instr)
        growTree :: ModuleDatum -> Modules (TreeContext, ModuleDatum)
        growTree mods = return (outcomesToContextTree mods, mods)
        gamifyTree :: (MonadMemo [(Opt, Opt, Double)] Result m) => (TreeContext, ModuleDatum) -> ModulesT m (TreeGame, ModuleDatum)
        gamifyTree (tree, mods) = do
            trees <- lift $ sequence . extend (foldTree $ treeScoreFolder mods) $ tree
            return (trees, mods)
        exportTree :: (TreeGame, ModuleDatum) -> ModulesT IO ()
        exportTree (tree, mods) = do
            let filename = makeValid . unpack . printpath $ mods
            liftIO . putStrLn $ "Exporting to out/" ++ filename
            liftIO . writeFile ("out/" ++ (unpack . printpath $ mods)) . unpack . (printdatum mods) $ tree
