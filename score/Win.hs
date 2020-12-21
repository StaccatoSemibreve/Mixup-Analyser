{-# LANGUAGE OverloadedStrings #-}

module Win
    ( score
    ) where

import Contexts
import Data.Function
import Data.Text (Text)
import Data.Map (Map)

score :: Context -> Double
score context = fromIntegral . ((-) `on` fromEnum . (< 1)) (getValue "BHealth" context) $ (getValue "AHealth" context)
