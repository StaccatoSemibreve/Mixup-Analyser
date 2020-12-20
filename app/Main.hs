{-# LANGUAGE OverloadedStrings #-}

module Main where

-- calculates nash equilibriums, does related backend stuff
import Game
-- parses yaml into specific types also stored here
import Parse
-- manipulates the yaml data into the relevant data trees, evaluates with a fold (well, a scan) using that
import Evaluate
-- contains the specialised functions for converting contexts into scores, updating context states, and checking if a game has ended
-- TODO: define this at runtime somehow, maybe use hint? idk! TODO then: take scores from Instructions to select score functions
import Custom
-- the helper functions used by Custom so that it's not also full of things that should never be altered
import Contexts

import Data.Text (unpack)
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
    let scoredata = map scores instructions
    let contexttrees = map (\(mgroup,instr) -> (outcomesToContextTree mgroup instr, scores instr)) . zip mgroups $ instructions
    let gametrees = concat . map (\(tree,scoreDatas) -> map (\scoreData -> (extend (foldTree $ treeScoreFolder scoreWin) tree, scoreData)) scoreDatas) $ contexttrees
    mapM_ (\(tree,scoreData) -> writeFile ("out/" ++ (unpack . outPath $ scoreData)) . drawTree . fmap prettyshownode $ tree) gametrees
    
    -- make sure i have something at the end so the do doesn't complain
    putStrLn "hlello wrorled"
    where
        prettyshownode (c, ("None1", _), ("None2", _), g) = (prettyshowgame g c)
        prettyshownode (c, (o1, _), ("None2", _), g) = (unpack o1) ++ ": " ++ (prettyshowgame g c)
        prettyshownode (c, ("None1", _), (o2, _), g) = (unpack o2) ++ ": " ++ (prettyshowgame g c)
        prettyshownode (c, (o1, _), (o2, _), g) = (unpack o1) ++ " + " ++ (unpack o2) ++ ": " ++ (prettyshowgame g c)
        
        prettyshowgame (GameComplex "" _ _ gdata (Just gout)) c = "\n" ++ (prettyshowcontext c) ++ prettyshowres gout
        prettyshowgame (GameComplex gname "" "" gdata (Just gout)) c = (unpack gname) ++ "\n" ++ (prettyshowcontext c) ++ "\n" ++ (prettyshowres gout)
        prettyshowgame (GameComplex gname attname defname gdata (Just gout)) c = (unpack gname) ++ "\n" ++ (prettyshowcontext c) ++ "\n" ++ " (" ++ (unpack attname) ++ " vs " ++ (unpack defname) ++ ")" ++ (prettyshowres gout)
        
        prettyshowcontext c = " Context: " ++ show c
        
        prettyshowres (ResultComplex ev sd [_] [_]) = "\n EV: " ++ (show ev) ++ "\n SD: " ++ (show sd)
        prettyshowres (ResultComplex ev sd [_] defs) = "\n EV: " ++ (show ev) ++ "\n SD: " ++ (show sd) ++ "\n Defender Options: " ++ (prettyshowouts defs)
        prettyshowres (ResultComplex ev sd atts [_]) = "\n EV: " ++ (show ev) ++ "\n SD: " ++ (show sd) ++ "\n Attacker Options: " ++ (prettyshowouts atts)
        prettyshowres (ResultComplex ev sd atts defs) = "\n EV: " ++ (show ev) ++ "\n SD: " ++ (show sd) ++ "\n Attacker Options: " ++ (prettyshowouts atts) ++ "\n Defender Options: " ++ (prettyshowouts defs)
        
        prettyshowouts outs = (\l -> foldl (\a b -> a ++ " - " ++ b) (head l) (tail l)) . map prettyshowpair $ outs
        prettyshowpair (opt, val) = (unpack opt) ++ ": " ++ (show val)
