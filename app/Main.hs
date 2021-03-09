{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

-- calculates nash equilibriums, does related backend stuff
import GameData
import Game
-- parses yaml into specific types also stored here
import ParseData
import Parse
-- manipulates the yaml data into the relevant data trees, evaluates with a fold (well, a scan) using that
import Evaluate
-- the helper functions used by Custom so that it's not also full of things that should never be altered
import Contexts
-- handles the score, endstate check, and context updating functions, defined at runtime
import ScoreData
import Score
-- handles the program arguments
import Args

import Data.Maybe
import Data.List
import Data.List.Split
import Data.Text (Text, unpack, pack)
import Data.Tree
import Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as HashMap
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad
import Control.Comonad
import System.Environment
import System.Directory (listDirectory)
import System.FilePath (makeValid)
import Control.Monad.Memo (MonadMemo, startEvalMemoT, runMemoStateT, evalMemoStateT)
import qualified Control.Monad.Memo as Memo
import Criterion.Main
import Control.Monad.Reader

main :: IO ()
main = do
    args <- getArgs >>= opts
    runReaderT program . flags . fst $ args

program :: FlagReader ()
program = do
    flags <- ask
    env <- environment
    let seeds = zip (runReader plantTrees env) (repeat flags)
    loggerF "Planted context trees!"
    lift . mapM_ (runReaderT treeThingy) $ seeds
    loggerF "Done!"
    where
        plantTrees :: Reader ModuleData [ModuleDatum]
        plantTrees = do
            instrs <- fmap instrdata ask
            unflatTrees <- mapM (\instr -> sequence . map (seed instr) . scores $ instr) instrs
            return . concat $ unflatTrees
            where
                seed :: Instruction -> ScoreData -> Reader ModuleData ModuleDatum
                seed instr sdata = do
                    mdata   <- fmap ($ path instr)                      $ getModule "Tried to get nonexistent input data" mixdata
                    fSA     <- fmap (\f -> map f $ scoreNamesAtt sdata) $ getModule "Tried to get nonexistent scores in" scoredata
                    fSD     <- fmap (\f -> map f $ scoreNamesDef sdata) $ getModule "Tried to get nonexistent scores in" scoredata
                    fE      <- fmap ($ endName sdata)                   $ getModule "Tried to get a nonexistent endstate" enddata
                    fU      <- fmap ($ updateName sdata)                $ getModule "Tried to get a nonexistent updater" updata
                    fP      <- fmap ($ outType sdata)                   $ getModule "Tried to get a nonexistent printer" printdata
                    
                    let fSAN = zip (scoreNamesAtt sdata) fSA
                        fSDN = zip (scoreNamesDef sdata) fSD
                        fEN  = (endName sdata, fE)
                        fUN  = (updateName sdata, fU)
                        fPN  = (outType sdata, fP)
                    
                    return $ ModuleDatum (name instr) mdata fSAN fSDN fEN fUN fPN (outPath sdata) (context instr)
        treeThingy :: ModuleReader IO ()
        treeThingy = do
            mods <- askMods
            flags <- askFlags
            growntree <- outcomesToContextTree
            logger $ "Grown context tree for " ++ (unpack . namedatum $ mods) ++ ", using its respective EndState and Updater modules!"
--             gametree <- startEvalMemoT . sequence . extend (foldTree treeScoreFolder) $ growntree
            memoedstuff <- (`runMemoStateT` HashMap.empty) . sequence . extend (foldTree treeScoreFolder) $ growntree
--             gametree <- (`evalMemoStateT` HashMap.empty) . sequence . extend (foldTree treeScoreFolder) $ growntree
            logger . ("Tree node count: "++) . show . HashMap.size . snd $ memoedstuff
            let gametree = fst memoedstuff
            logger "Analysed context tree, using its Score module!"
            let filename = makeValid . unpack . printpath $ mods
            logger $ "Exporting to " ++ (flagOut flags) ++ "/" ++ filename ++ " using its Printer module!"
            printed <- printTree gametree
            writer (unpack . printpath $ mods) . unpack $ printed
