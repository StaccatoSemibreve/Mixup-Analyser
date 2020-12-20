{-# LANGUAGE OverloadedStrings #-}

-- contains the specialised functions for converting contexts into scores, updating context states, and checking if a game has ended
-- TODO: define this at runtime somehow, maybe use hint? idk
module Custom
    ( contextCheck
    , endCheck
    , score
    ) where

import Contexts

import Data.List
import Data.Function
import Data.Text (Text)

contextCheck :: Context -> Context
contextCheck = chipCheck "BHealth" "BChip" "" . chipCheck "AHealth" "AChip" ""
    where
        chipCheck :: Text -> Text -> Text -> Context -> Context
        chipCheck healthkey chipkey guardbreakkey context = do
            let chip = getValue chipkey context
            let guardbreak = getValueDefault guardbreakkey 1 context
            let chippedContext = case (chip < 3) of True -> context
                                                    _ -> do
                                                        setValue (chipkey, rem chip 3) . addValue (healthkey, ((negate . quot chip $ 3) * guardbreak)) $ context
            let unguardbreakContext = case (guardbreak == 1) of True -> chippedContext
                                                                _ -> setValue (guardbreakkey, 1) chippedContext
            unguardbreakContext

endCheck :: Context -> Bool
endCheck context = or [getValue "AHealth" context <= 0, getValue "BHealth" context <= 0]

score :: Context -> Double
score = scoreWin

-- scoreHealthDifferential :: Context -> Double
-- scoreHealthDifferential context = 

scoreWin :: Context -> Double
scoreWin context = fromIntegral . ((-) `on` fromEnum . (< 1)) (getValue "BHealth" context) $ (getValue "AHealth" context)
