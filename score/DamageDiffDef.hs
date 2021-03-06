{-# LANGUAGE OverloadedStrings #-}

module DamageDiffDef
    ( score
    ) where

import Contexts
import Data.Function
import Data.Text (Text)
import Data.Map (Map)
import Control.Monad.State.Lazy (State)

score :: ContextS Double
score = do
    starthealthA <- fmap fromIntegral $ getValue "AStartHealth"
    starthealthB <- fmap fromIntegral $ getValue "BStartHealth"
    healthA <- fmap fromIntegral $ getValue "AHealth"
    healthB <- fmap fromIntegral $ getValue "BHealth"
    chipA <- fmap fromIntegral $ getValue "AChip"
    chipB <- fmap fromIntegral $ getValue "BChip"
    
    return $ ((healthB - starthealthB) - chipB/3) - ((healthA - starthealthA) - chipA/3)
