{-# LANGUAGE OverloadedStrings #-}

module DamageDiff
    ( score
    ) where

import Contexts
import Data.Function
import Data.Text (Text)
import Data.Map (Map)

score :: Context -> Double
score context =
    let
        healthA = fromIntegral $ getValue "AHealth" context
        healthB = fromIntegral $ getValue "BHealth" context
        chipA = fromIntegral $ getValue "AChip" context
        chipB = fromIntegral $ getValue "BChip" context
    in
        case (healthA > 0, healthB > 0) of
             (_, _) -> (healthA - chipA/3) - (healthB - chipB/3)
