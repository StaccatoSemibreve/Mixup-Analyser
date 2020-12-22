{-# LANGUAGE OverloadedStrings #-}

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
import Control.Monad
import Control.Comonad
import System.Directory (listDirectory)
import System.FilePath (makeValid)

main :: IO ()
main =  do
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
    
    instructions <- readInstructions
    if null instructions then error "No instructions found in config.yaml!" else putStrLn "Found instructions in config.yaml:"
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
    putStrLn "Parsed and compiled all required Score modules!"
    parsedEnds <- mapM (parseScript end) reqEnds
    putStrLn "Parsed and compiled all required EndState modules!"
    parsedUpdaters <- mapM (parseScript update) reqUpdaters
    putStrLn "Parsed and compiled all required Updater modules!"
    parsedPrinters <- mapM (parseScript printer) reqPrinters
    putStrLn "Parsed and compiled all required Printer modules!"
    parsedData <- mapM parseData reqData
    putStrLn "Parsed all required input data!"
    
    let scoreLookup = zip (map pack reqScores) parsedScores
    let endLookup = zip (map pack reqEnds) parsedEnds
    let updaterLookup = zip (map pack reqUpdaters) parsedUpdaters
    let printerLookup = zip (map pack reqPrinters) parsedPrinters
    let dataLookup = zip (map pack reqData) parsedData
    putStrLn "Created lookup tables!"
    
    
    let roots = concat . map (plantTrees dataLookup) $ instructions
    putStrLn "Planted context trees!"
    let contexttrees = map (growTree endLookup updaterLookup) roots
    putStrLn "Grown context trees, using their respective EndState and Updater modules!"
    let gametrees = map (gamifyTree scoreLookup) contexttrees
    putStrLn "Analysed context trees, using their respective Score modules!"
    
    mapM_ (exportTree printerLookup) gametrees
    putStrLn "Done!"
    where
        findFiles :: String -> IO ([String])
        findFiles = fmap (map $ concat . init . splitOn ".") . listDirectory . makeValid
        
        parseScript :: Show a => (String -> IO (Either a b)) -> String -> IO b
        parseScript f = fmap (either (\e -> error . show $ e) id) . f
        parseScore :: ScoreData -> IO (Context -> Double)
        parseScore sdata = fmap (either (\e -> error . show $ e) id) . score . unpack . scoreName $ sdata
        parseEnd :: ScoreData -> IO (Context -> Bool)
        parseEnd sdata = fmap (either (\e -> error . show $ e) id) . end . unpack . endName $ sdata
        parseUpdate :: ScoreData -> IO (Context -> Context)
        parseUpdate sdata = fmap (either (\e -> error . show $ e) id) . update . unpack . updateName $ sdata
        parsePrinter :: ScoreData -> IO (TreeGame -> Text)
        parsePrinter sdata = fmap (either (\e -> error . show $ e) id) . printer . unpack . outType $ sdata
        
        getScore :: [(Text, Context -> Double)] -> Text -> (Context -> Double)
        getScore fs f = fromMaybe (error $ "Tried to get a nonexistent score: " ++ (unpack f)) . lookup f $ fs
        getEnd :: [(Text, Context -> Bool)] -> Text -> (Context -> Bool)
        getEnd fs f = fromMaybe (error $ "Tried to get a nonexistent endstate: " ++ (unpack f)) . lookup f $ fs
        getUpdater :: [(Text, Context -> Context)] -> Text -> (Context -> Context)
        getUpdater fs f = fromMaybe (error $ "Tried to get a nonexistent updater: " ++ (unpack f)) . lookup f $ fs
        getPrinter :: [(Text, TreeGame -> Text)] -> Text -> (TreeGame -> Text)
        getPrinter fs f = fromMaybe (error $ "Tried to get a nonexistent printer: " ++ (unpack f)) . lookup f $ fs
        getData :: [(Text, [MixupGroup])] -> Text -> [MixupGroup]
        getData fs f = fromMaybe (error $ "Tried to get nonexistent input data: " ++ (unpack f)) . lookup f $ fs
        
        plantTrees :: [(Text, [MixupGroup])] -> Instruction -> [([MixupGroup], Instruction, ScoreData)]
        plantTrees mgroups instr = map (\s -> (getData mgroups . path $ instr, instr, s)) . scores $ instr
        growTree :: [(Text, Context -> Bool)] -> [(Text, Context -> Context)] -> ([MixupGroup], Instruction, ScoreData) -> (TreeContext, Instruction, ScoreData)
        growTree fs1 fs2 (mgroup, instr, sdata) = (outcomesToContextTree mgroup (getEnd fs1 . endName $ sdata) (getUpdater fs2 . updateName $ sdata) instr, instr, sdata)
        gamifyTree :: [(Text, Context -> Double)] -> (TreeContext, Instruction, ScoreData) -> (TreeGame, ScoreData)
        gamifyTree fs (tree, instr, sdata) = (extend (foldTree $ treeScoreFolder (getScore fs . scoreName $ sdata)) tree, sdata)
        exportTree :: [(Text, TreeGame -> Text)] -> (TreeGame, ScoreData) -> IO ()
        exportTree fs (tree, sdata) = do
            let filename = makeValid . unpack . outPath $ sdata
            putStrLn $ "Exporting to out/" ++ filename
            let treedepth = foldTree treeDepthFolder $ tree
            putStrLn $ "Tree depth: " ++ (show treedepth)
            when (treedepth > 10000) $ putStrLn "Oh no…"
            putStrLn $ "Printer: " ++ (unpack . outType $ sdata)
            writeFile ("out/" ++ (unpack . outPath $ sdata)) . unpack . (getPrinter fs . outType $ sdata) $ tree
            where
                treeDepthFolder :: a -> [Int] -> Int
                treeDepthFolder _ [] = 1
                treeDepthFolder _ xs = sum xs
