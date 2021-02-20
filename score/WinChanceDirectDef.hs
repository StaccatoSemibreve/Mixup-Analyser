{-# LANGUAGE OverloadedStrings #-}

module WinChanceDirectDef
    ( score
    ) where

import Contexts
import Data.Function
import Data.Text (Text)
import Data.Map (Map)
import Control.Monad.State.Lazy (State)

score :: ContextS Double
score = fmap (1-) . fmap (/100) . fmap fromInteger $ getValue "WinA"
