{-# LANGUAGE OverloadedStrings #-}

module Chipper
    ( updater
    ) where

import Prelude
import Contexts
import Data.Text (Text)
import Data.Map (Map)
import Control.Monad
import Control.Monad.State.Lazy (StateT)

healthAStart = "AStartHealth"
healthA = "AHealth"
chipA = "AChip"
guardA = "AGuardbreakDamage"
healthBStart = "BStartHealth"
healthB = "BHealth"
chipB = "BChip"
guardB = "BGuardbreakDamage"

updater :: ContextS ()
updater = do
    firstRun <- hasValue "start"
    when firstRun startCheck
    chipCheck healthA chipA guardB
    chipCheck healthB chipB guardA
    where
        startCheck :: ContextS ()
        startCheck = do
            startA <- getValue healthAStart
            startB <- getValue healthBStart
            
            setValue healthA startA
            setValue healthB startB
            removeValue "start"
        
        chipCheck :: Text -> Text -> Text -> ContextS ()
        chipCheck healthkey chipkey guardkey = do
            chip <- getValue chipkey
            guardbreak <- getValueDefault guardkey 1
            when (chip >= 3) $ do
                addValue healthkey . (*guardbreak) . negate . quot chip $ 3
                setValue chipkey $ rem chip 3
            removeValue guardkey
