{-# LANGUAGE OverloadedStrings #-}

module DamageDiff
    ( score
    ) where

import Contexts
import Data.Function
import Data.Text (Text)
import Data.Map (Map)

score :: Context -> Double
score context = fromIntegral . subtract (getValue "BHealth" context) $ (getValue "AHealth" context)
