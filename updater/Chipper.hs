{-# LANGUAGE OverloadedStrings #-}

module Chipper
    ( update
    ) where

import Contexts
import Data.Text (Text)
import Data.Map (Map)

update :: Context -> Context
update context = chipCheck "BHealth" "BChip" "AGuardbreakDamage" . chipCheck "AHealth" "AChip" "BGuardbreakDamage" $ context
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
