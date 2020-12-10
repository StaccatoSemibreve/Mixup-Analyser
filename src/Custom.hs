{-# LANGUAGE OverloadedStrings #-}

-- contains the specialised functions for converting contexts into scores, updating context states, and checking if a game has ended
-- TODO: define this at runtime somehow, maybe use hint? idk
module Custom
    ( contextCheck
    , endCheck
    , score
    ) where

import Helpers
import Data.List
import Data.Function
import Data.Text (Text)

healthAtt = "AHealth"
healthDef = "BHealth"
chipAtt = "AChip"
chipDef = "BChip"
superAtt = "ASuper"
superDef = "BSuper"
guardbreakAtt = ""
guardbreakDef = ""

contextCheck :: Context -> Context
contextCheck = chipCheck healthDef chipDef guardbreakDef . chipCheck healthAtt chipAtt guardbreakAtt
    where
        chipCheck :: Text -> Text -> Text -> Context -> Context
        chipCheck healthkey chipkey guardbreakkey context = do
            let chip = getContextValue chipkey context
            let guardbreak = getContextValueDefault guardbreakkey 1 context
            let chippedContext = case (chip < 3) of True -> context
                                                    _ -> do
                                                        setContextValue (chipkey, rem chip 3) . addContextValue (healthkey, ((negate . quot chip $ 3) * guardbreak)) $ context
            let unguardbreakContext = case (guardbreak == 1) of True -> chippedContext
                                                                _ -> setContextValue (guardbreakkey, 1) chippedContext
            unguardbreakContext

endCheck :: Context -> Bool
endCheck context = ((maybe 0 id . lookup healthAtt $ context) <= 0) || ((maybe 0 id . lookup healthDef $ context) <= 0)

score :: Context -> Double
score = scoreWin

-- scoreHealthDifferential :: Context -> Double
-- scoreHealthDifferential context = 

scoreWin :: Context -> Double
scoreWin context = fromIntegral . ((-) `on` fromEnum . (== 0)) (getContextValue healthDef context) $ (getContextValue healthAtt context)
