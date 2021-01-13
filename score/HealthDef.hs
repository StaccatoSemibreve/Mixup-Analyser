{-# LANGUAGE OverloadedStrings #-}

module HealthDef
    ( score
    ) where

import Contexts
import Data.Function
import Data.Text (Text)
import Data.Map (Map)

score :: ContextS Double
score = do
    bhealth <- fmap (fromIntegral) $ getValue "BHealth"
    bhealthstart <- fmap (fromIntegral) $ getValueDefault "BStartHealth" 6
    return $ bhealth - bhealthstart
