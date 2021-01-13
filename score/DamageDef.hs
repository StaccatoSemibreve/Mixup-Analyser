{-# LANGUAGE OverloadedStrings #-}

module DamageDef
    ( score
    ) where

import Contexts
import Data.Function
import Data.Text (Text)
import Data.Map (Map)

score :: ContextS Double
score = do
    ahealth <- fmap (fromIntegral) $ getValue "AHealth"
    ahealthstart <- fmap (fromIntegral) $ getValueDefault "AStartHealth" 6
    return $ ahealthstart - ahealth
