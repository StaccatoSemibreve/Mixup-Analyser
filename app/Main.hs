module Main where

-- calculates nash equilibriums, does related backend stuff
import Game
-- parses yaml into specific types also stored here
import Parse
-- manipulates the yaml data into the relevant data trees, evaluates from that
-- TODO: finish
import Evaluate
-- contains the specialised functions for converting contexts into scores, updating context states, and checking if a game has ended
-- TODO: define this at runtime somehow, maybe use hint? idk
import Custom
-- the helper functions used by Custom so that it's not also full of things that should never be altered
import Helpers

main :: IO ()
main =  do
    -- old test cases for the Game module
--     let gs = [game "Strike/Throw" ["Strike","Throw"] ["Block", "Yomi"] [[0.3,1], [2,-1]],
--               game "Strike/Throw (Borked)" ["Strike","Strike"] ["Block", "Yomi"] [[0.3,0.3], [2,2]],
--               game "Strike/Throw (Antiblock)" ["Strike","Throw"] ["Block", "Yomi", "Antiblock"] [[0.3,1], [2,-1], [1,0.3]],
--               game "Strike" ["Strike"] ["Block", "Yomi"] [[0.3], [2]],
--               game "Oki (Rook)" ["Strike","Throw","Cmd"] ["Block","Yomi","Jump"] [[0.3,1,1], [2,-1,2], [1,2,-3]]]
--     mapM_ putStrLn . map (show . solve) $ gs
    
    -- current test case from Evaluate
    test
    -- make sure i have something at the end so the do doesn't complain
    putStrLn "hlello wrorled"
