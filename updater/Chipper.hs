{-# LANGUAGE OverloadedStrings #-}

module Chipper
    ( update
    ) where

import Contexts
import Data.Text (Text)
import Data.Map (Map)

healthAStart = "AStartHealth"
healthA = "AHealth"
chipA = "AChip"
guardA = "AGuardbreakDamage"
healthBStart = "BStartHealth"
healthB = "BHealth"
chipB = "BChip"
guardB = "BGuardbreakDamage"

update :: Context -> Context
update context = chipCheck healthB chipB guardA . chipCheck healthA chipA guardB . startCheck $ context
    where
        startCheck :: Context -> Context
        startCheck context =
            case (hasValue healthA context, hasValue healthB context) of
                 (True, True) -> context
                 (False, True) -> setValue (healthA, (getValue healthAStart context)) context
                 (True, False) -> setValue (healthB, (getValue healthBStart context)) context
                 (False, False) -> setValue (healthB, (getValue healthBStart context)) . setValue (healthA, (getValue healthAStart context)) $ context
        
        chipCheck :: Text -> Text -> Text -> Context -> Context
        chipCheck healthkey chipkey guardbreakkey context =
            let
                chip = getValue chipkey context
                guardbreak = getValueDefault guardbreakkey 1 context
                chippedContext =
                    case (chip < 3) of
                         True -> context
                         _ -> setValue (chipkey, rem chip 3) . addValue (healthkey, ((negate . quot chip $ 3) * guardbreak)) $ context
                unguardbreakContext =
                    case (guardbreak == 1) of
                         True -> chippedContext
                         _ -> setValue (guardbreakkey, 1) chippedContext
            in
                unguardbreakContext
