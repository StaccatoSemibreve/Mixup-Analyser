{-# LANGUAGE OverloadedStrings #-}

-- the helper functions used by Custom so that it's not also full of things that should never be altered
module Helpers
    ( setContextValue
    , addContextValue
    , getContextValue
    , getContextValueDefault
    , Context
    ) where

import Data.List
import Data.Function
import Data.Text (Text)

type Context = [(Text, Integer)]

setContextValue :: (Text, Integer) -> Context -> Context
setContextValue (k,v) c = (k,v):(deleteFirstsBy ((==) `on` fst) c [(k,v)])

addContextValue :: (Text, Integer) -> Context -> Context
addContextValue (k,v) c = setContextValue (k, v + (maybe 0 id . lookup k $ c)) c

getContextValueDefault :: Text -> Integer -> Context -> Integer
getContextValueDefault k d = maybe d id . lookup k

getContextValue :: Text -> Context -> Integer
getContextValue k = getContextValueDefault k 0
