{-# LANGUAGE OverloadedStrings #-}

module HealthZero
    ( end
    ) where

import Contexts
import Data.Text (Text)
import Data.Map (Map)
import Control.Monad.State.Lazy (State)

end :: ContextS Bool
end = do
    healthA <- getValue "AHealth"
    healthB <- getValue "BHealth"
    return $ or [healthA <= 0, healthB <= 0]
