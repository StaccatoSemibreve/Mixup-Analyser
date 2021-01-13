{-# LANGUAGE OverloadedStrings #-}

module Win
    ( score
    ) where

import Contexts
import Data.Function
import Data.Text (Text)
import Data.Map (Map)
import Control.Monad.State.Lazy (State)

score :: ContextS Double
score = do
    healthA <- getValue "AHealth"
    healthB <- getValue "BHealth"
    return . fromIntegral $ ((-) `on` fromEnum . (< 1)) healthB healthA
