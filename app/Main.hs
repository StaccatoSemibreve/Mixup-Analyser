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
import Control.Monad.Memo (MonadMemo, startEvalMemoT)
import qualified Control.Monad.Memo as Memo
import Criterion.Main
import Control.Monad.Reader

main :: IO ()
main = do
    args <- getArgs >>= opts
    runReaderT program . flags . fst $ args

program :: FlagReader ()
program = do
    env <- environment
    let seeds = runReader plantTrees env
    logger "Planted context trees!"
    mapM_ (runReaderT treeThingy) $ seeds
    logger "Done!"
    where
        findFiles :: String -> IO ([String])
        findFiles = fmap (map $ concat . init . splitOn ".") . listDirectory . makeValid
        
        plantTrees :: Reader ModuleData [ModuleDatum]
        plantTrees = do
            instrs <- fmap instrdata ask
            unflatTrees <- mapM (\instr -> sequence . map (seed instr) . scores $ instr) instrs
            return . concat $ unflatTrees
            where
                seed :: Instruction -> ScoreData -> Reader ModuleData ModuleDatum
                seed instr sdata = do
                    mdata   <- fmap ($ path instr) $ getModule "Tried to get nonexistent input data" mixdata
                    fS      <- fmap ($ scoreName sdata) $ getModule "Tried to get a nonexistent score" scoredata
                    fE      <- fmap ($ endName sdata) $ getModule "Tried to get a nonexistent endstate" enddata
                    fU      <- fmap ($ updateName sdata) $ getModule "Tried to get a nonexistent updater" updata
                    fP      <- fmap ($ outType sdata) $ getModule "Tried to get a nonexistent printer" printdata
                    return $ ModuleDatum (name instr) mdata fS fE fU fP (outPath sdata) (context instr)
        growTree :: ModuleReader TreeContext
        growTree = asks outcomesToContextTree
        gamifyTree :: TreeContext -> TreeMemoT ModuleReader TreeGame
        gamifyTree = sequence . extend (foldTree treeScoreFolder)
        exportTree :: TreeGame -> ModuleReader ()
        exportTree tree = do
            mods <- ask
            let filename = makeValid . unpack . printpath $ mods
            lift $ logger $ "Exporting to out/" ++ filename
            lift $ writer (unpack . printpath $ mods) . unpack . (printdatum mods) $ tree
        treeThingy :: ModuleReader ()
        treeThingy = do
            mods <- ask
            growntree <- growTree
            lift $ logger $ "Grown context tree for " ++ (unpack . namedatum $ mods) ++ ", using its respective EndState and Updater modules!"
            gametree <- startEvalMemoT . gamifyTree $ growntree
            lift $ logger "Analysed context trees, using their respective Score modules! The next step may take a moment to begin."
            exportTree gametree
