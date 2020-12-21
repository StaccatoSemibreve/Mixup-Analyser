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
import Data.Text (Text, unpack)
import Data.Tree
import Control.Comonad

main :: IO ()
main =  do
    -- old test cases for the Game module
--     let gs = [game "Strike/Throw" ["Strike","Throw"] ["Block", "Yomi"] [[0.3,1], [2,-1]],
--               game "Strike/Throw (Borked)" ["Strike","Strike"] ["Block", "Yomi"] [[0.3,0.3], [2,2]],
--               game "Strike/Throw (Antiblock)" ["Strike","Throw"] ["Block", "Yomi", "Antiblock"] [[0.3,1], [2,-1], [1,0.3]],
--               game "Strike" ["Strike"] ["Block", "Yomi"] [[0.3], [2]],
--               game "Oki (Rook)" ["Strike","Throw","Cmd"] ["Block","Yomi","Jump"] [[0.3,1,1], [2,-1,2], [1,2,-3]],
--               game "weird" ["Strike"] ["Block", "Yomi"] [[0.3],[2]] ]
--     mapM_ putStrLn . map (show . solve) $ gs
    
--     let g = gameComplex "Strike/Throw" "" "" [(("Strike", Nothing), ("Block", Just 0.81), 0.3), (("Strike", Nothing), ("Yomi", Just 0.19), 2), (("Strike", Nothing), ("Superblock", Just 0), 1)
--                                              ,(("Throw", Nothing), ("Block", Just 0.81), 1), (("Throw", Nothing), ("Yomi", Just 0.19), -1), (("Throw", Nothing), ("Superblock", Just 0), -1)]
--     let g = gameComplex "Strike/Item" [(("Coin", Just 0.75), ("Block", Nothing), 0.3), (("Coin", Just 0.75), ("Yomi", Nothing), 1),
--                                        (("Cherry", Just 0.25), ("Block", Nothing), 1), (("Cherry", Just 0.25), ("Yomi", Nothing), -1)]
--     
--     let gsolved = solveComplex g
--     putStrLn . show $ gsolved
    
    instructions <- readInstructions
    mgroups <- mapM instructionToMixupGroups instructions
    
    
    let scoredata = concat . map scores $ instructions
    
    parsedScores <- mapM parseScore scoredata
    parsedEnds <- mapM parseEnd scoredata
    parsedUpdaters <- mapM parseUpdate scoredata
    parsedPrinters <- mapM parsePrinter scoredata
    
    let scoreLookup = zip (map scoreName scoredata) parsedScores
    let endLookup = zip (map endName scoredata) parsedEnds
    let updaterLookup = zip (map updateName scoredata) parsedUpdaters
    let printerLookup = zip (map outType scoredata) parsedPrinters
    
    
    let roots = concat . map treeRoots . zip mgroups $ instructions
    let contexttrees = map (growTree endLookup updaterLookup) roots
    let gametrees = map (gamifyTree scoreLookup) contexttrees
    
    mapM_ (\(tree,sdata) -> writeFile ("out/" ++ (unpack . outPath $ sdata)) . (getPrinter printerLookup . outType $ sdata) $ tree) gametrees
    where
        parseScore :: ScoreData -> IO (Context -> Double)
        parseScore sdata = fmap (either (\e -> error . show $ e) id) . score . unpack . scoreName $ sdata
        parseEnd :: ScoreData -> IO (Context -> Bool)
        parseEnd sdata = fmap (either (\e -> error . show $ e) id) . end . unpack . endName $ sdata
        parseUpdate :: ScoreData -> IO (Context -> Context)
        parseUpdate sdata = fmap (either (\e -> error . show $ e) id) . update . unpack . updateName $ sdata
        parsePrinter :: ScoreData -> IO (TreeGame -> String)
        parsePrinter sdata = fmap (either (\e -> error . show $ e) id) . printer . unpack . outType $ sdata
        
        getScore :: [(Text, Context -> Double)] -> Text -> (Context -> Double)
        getScore fs f = fromMaybe (error $ "tried to get a nonexistent score: " ++ (unpack f)) . lookup f $ fs
        getEnd :: [(Text, Context -> Bool)] -> Text -> (Context -> Bool)
        getEnd fs f = fromMaybe (error $ "tried to get a nonexistent endstate: " ++ (unpack f)) . lookup f $ fs
        getUpdater :: [(Text, Context -> Context)] -> Text -> (Context -> Context)
        getUpdater fs f = fromMaybe (error $ "tried to get a nonexistent updater: " ++ (unpack f)) . lookup f $ fs
        getPrinter :: [(Text, TreeGame -> String)] -> Text -> (TreeGame -> String)
        getPrinter fs f = fromMaybe (error $ "tried to get a nonexistent printer: " ++ (unpack f)) . lookup f $ fs
        
        treeRoots :: ([MixupGroup], Instruction) -> [([MixupGroup], Instruction, ScoreData)]
        treeRoots (mgroup, instr) = map (\s -> (mgroup, instr, s)) . scores $ instr
        growTree :: [(Text, Context -> Bool)] -> [(Text, Context -> Context)] -> ([MixupGroup], Instruction, ScoreData) -> (TreeContext, Instruction, ScoreData)
        growTree fs1 fs2 (mgroup, instr, sdata) = (outcomesToContextTree mgroup (getEnd fs1 . endName $ sdata) (getUpdater fs2 . updateName $ sdata) instr, instr, sdata)
        gamifyTree :: [(Text, Context -> Double)] -> (TreeContext, Instruction, ScoreData) -> (TreeGame, ScoreData)
        gamifyTree fs (tree, instr, sdata) = (extend (foldTree $ treeScoreFolder (getScore fs . scoreName $ sdata)) tree, sdata)
