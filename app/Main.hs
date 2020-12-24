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

import Data.Maybe
import Data.List
import Data.List.Split
import Data.Text (Text, unpack, pack)
import Data.Tree
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad
import Control.Comonad
import System.Directory (listDirectory)
import System.FilePath (makeValid)
import Control.Monad.Memo (MonadMemo, startEvalMemo)
import qualified Control.Monad.Memo as Memo
import Criterion.Main

main :: IO ()
main = program "config.yaml"

program :: String -> IO ()
program config = do
    foundScores     <- findFiles "score"
    foundEnds       <- findFiles "endstate"
    foundUpdaters   <- findFiles "updater"
    foundPrinters   <- findFiles "printer"
    foundData       <- findFiles "in"
    
    if null foundScores then error "No Score modules found in /score!" else putStrLn "Found Score modules in /score:"
    mapM_ putStrLn . map ("- "++) $ foundScores
    
    if null foundEnds then error "No EndState modules found in /endstate!" else putStrLn "Found EndState modules in /endstate:"
    mapM_ putStrLn . map ("- "++) $ foundEnds
    
    if null foundUpdaters then error "No Updater modules found in /updater!" else putStrLn "Found Updater modules in /updater:"
    mapM_ putStrLn . map ("- "++) $ foundUpdaters
    
    if null foundPrinters then error "No Printer modules found in /printer!" else putStrLn "Found Printer modules in /printer:"
    mapM_ putStrLn . map ("- "++) $ foundPrinters
    
    if null foundData then error "No input data found in /in!" else putStrLn "Found input data in /in:"
    mapM_ putStrLn . map ("- "++) $ foundData
    
    instructions <- readInstructions config
    if null instructions then error ("No instructions found in " ++ config ++ "!") else putStrLn "Found instructions in config.yaml:"
    mapM_ putStrLn . map (\instr -> "- "++(unpack . name $ instr)) $ instructions
    
    let instrScoreData = nub . map scores $ instructions
    let instrScores = nub . concat . map (map $ unpack . scoreName) $ instrScoreData
    let instrEnds = nub . concat . map (map $ unpack . endName) $ instrScoreData
    let instrUpdaters = nub . concat . map (map $ unpack . updateName) $ instrScoreData
    let instrPrinters = nub . concat . map (map $ unpack . outType) $ instrScoreData
    let instrData = nub . (map $ unpack . path) $ instructions
    putStrLn "Read instructions!"
    
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
    
    
    let roots = concat . map (plantTrees dataLookup) $ instructions
    putStrLn "Planted context trees!"
    let contexttrees = map (growTree endLookup updaterLookup) roots
    putStrLn "Grown context trees, using their respective EndState and Updater modules!"
--     defaultMain [
--         bgroup "gamifyTree" [ bench "new" $ nf (map (fmap (resCEV . fromMaybe (error "???") . outcomesC . (\(_,_,_,x) -> x)) . fst) . startEvalMemo . traverse (gamifyTree scoreLookup)) contexttrees
--                             , bench "oldish" $ nf (map (fmap (resCEV . fromMaybe (error "???") . outcomesC . (\(_,_,_,x) -> x)) . fst . gamifyTreeOldish scoreLookup)) contexttrees
--                             ]
--                 ]
    let gametrees = startEvalMemo . traverse (gamifyTree scoreLookup) $ contexttrees
    putStrLn $ "Analysed context trees, using their respective Score modules! The next step may take a moment to begin."
    
    mapM_ (exportTree printerLookup) gametrees
    putStrLn "Done!"
    where
        findFiles :: String -> IO ([String])
        findFiles = fmap (map $ concat . init . splitOn ".") . listDirectory . makeValid
        
        parseScript :: Show a => (String -> IO (Either a b)) -> String -> IO b
        parseScript f = fmap (either (\e -> error . show $ e) id) . f
        parseScore :: ScoreData -> IO Score
        parseScore sdata = fmap (either (\e -> error . show $ e) id) . score . unpack . scoreName $ sdata
        parseEnd :: ScoreData -> IO EndState
        parseEnd sdata = fmap (either (\e -> error . show $ e) id) . end . unpack . endName $ sdata
        parseUpdate :: ScoreData -> IO Updater
        parseUpdate sdata = fmap (either (\e -> error . show $ e) id) . update . unpack . updateName $ sdata
        parsePrinter :: ScoreData -> IO Printer
        parsePrinter sdata = fmap (either (\e -> error . show $ e) id) . printer . unpack . outType $ sdata
        
        getScore :: Map Text Score -> Text -> Score
        getScore fs f = fromMaybe (error $ "Tried to get a nonexistent score: " ++ (unpack f)) . Map.lookup f $ fs
        getEnd :: Map Text EndState -> Text -> EndState
        getEnd fs f = fromMaybe (error $ "Tried to get a nonexistent endstate: " ++ (unpack f)) . Map.lookup f $ fs
        getUpdater :: Map Text Updater -> Text -> Updater
        getUpdater fs f = fromMaybe (error $ "Tried to get a nonexistent updater: " ++ (unpack f)) . Map.lookup f $ fs
        getPrinter :: Map Text Printer -> Text -> Printer
        getPrinter fs f = fromMaybe (error $ "Tried to get a nonexistent printer: " ++ (unpack f)) . Map.lookup f $ fs
        getData :: Map Text MixupData -> Text -> MixupData
        getData fs f = fromMaybe (error $ "Tried to get nonexistent input data: " ++ (unpack f)) . Map.lookup f $ fs
        
        plantTrees :: Map Text MixupData -> Instruction -> [(MixupData, Instruction, ScoreData)]
        plantTrees mgroups instr = map (\s -> (getData mgroups . path $ instr, instr, s)) . scores $ instr
        growTree :: Map Text EndState -> Map Text Updater -> (MixupData, Instruction, ScoreData) -> (TreeContext, Instruction, ScoreData)
        growTree fs1 fs2 (mgroup, instr, sdata) = (outcomesToContextTree mgroup (getEnd fs1 . endName $ sdata) (getUpdater fs2 . updateName $ sdata) instr, instr, sdata)
        gamifyTree :: (MonadMemo [(Opt, Opt, Double)] Result m) => Map Text Score -> (TreeContext, Instruction, ScoreData) -> m (TreeGame, ScoreData)
        gamifyTree fs (tree, instr, sdata) = (sequence . extend (foldTree $ treeScoreFolderM (getScore fs . scoreName $ sdata)) $ tree) >>= (\trees -> return (trees, sdata))
        gamifyTreeOldish :: Map Text Score -> (TreeContext, Instruction, ScoreData) -> (TreeGame, ScoreData)
        gamifyTreeOldish fs (tree, instr, sdata) = (startEvalMemo . sequence . extend (foldTree $ treeScoreFolderM (getScore fs . scoreName $ sdata)) $ tree, sdata)
        gamifyTreeOld :: Map Text Score -> (TreeContext, Instruction, ScoreData) -> (TreeGame, ScoreData)
        gamifyTreeOld fs (tree, instr, sdata) = (extend (foldTree $ treeScoreFolder (getScore fs . scoreName $ sdata)) tree, sdata)
        exportTree :: Map Text Printer -> (TreeGame, ScoreData) -> IO ()
        exportTree fs (tree, sdata) = do
            let filename = makeValid . unpack . outPath $ sdata
            putStrLn $ "Exporting to out/" ++ filename
--             let treedepth = foldTree treeDepthFolder $ tree
--             putStrLn $ "Tree depth: " ++ (show treedepth)
--             when (treedepth > 10000) $ putStrLn "Oh no…"
            putStrLn $ "Printer: " ++ (unpack . outType $ sdata)
            writeFile ("out/" ++ (unpack . outPath $ sdata)) . unpack . (getPrinter fs . outType $ sdata) $ tree
            where
                treeDepthFolder :: a -> [Int] -> Int
                treeDepthFolder _ [] = 1
                treeDepthFolder _ xs = sum xs
