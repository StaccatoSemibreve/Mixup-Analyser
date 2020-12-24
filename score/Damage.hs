{-# LANGUAGE OverloadedStrings #-}

module Damage
    ( score
    ) where

import Contexts
import Data.Function
import Data.Text (Text)
import Data.Map (Map)

score :: Context -> Double
score context = fromIntegral . negate $ getValue "BHealth" context
