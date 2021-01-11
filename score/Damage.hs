{-# LANGUAGE OverloadedStrings #-}

module Damage
    ( score
    ) where

import Contexts
import Data.Function
import Data.Text (Text)
import Data.Map (Map)

score :: Context -> Double
score context = fromIntegral (getValue "BStartHealth" context - getValue "BHealth" context)
