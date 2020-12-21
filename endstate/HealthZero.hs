{-# LANGUAGE OverloadedStrings #-}

module HealthZero
    ( end
    ) where

import Contexts
import Data.Text (Text)
import Data.Map (Map)

end :: Context -> Bool
end context = or [getValue "AHealth" context <= 0, getValue "BHealth" context <= 0]
