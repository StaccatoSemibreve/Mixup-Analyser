{-# LANGUAGE OverloadedStrings #-}

-- the helper functions used by Custom so that it's not also full of things that should never be altered
module Contexts
    ( setValue
    , sets
    , addValue
    , adds
    , addset
    , getValue
    , getValueDefault
    , getValueMaybe
    , compareValue
    , compareValueDefault
    , compareValueMaybe
    , compareAll
    , compareNone
    , newContext
    , Context
    ) where

import Data.List
import Data.Function
import Data.Maybe
import Data.Text (Text)
import Data.Map (Map)
import qualified Data.Map as Map

type Context = Map Text Integer

setValue :: (Text, Integer) -> Context -> Context
setValue (k,v) c = Map.insert k v c

sets :: Context -> Context -> Context
sets = Map.union

addValue :: (Text, Integer) -> Context -> Context
addValue (k,v) c = Map.insertWith (+) k v c

adds :: Context -> Context -> Context
adds = Map.unionWith (+)

addset :: Context -> Context -> Context -> Context
addset s a c = adds a . sets s $ c

getValueMaybe :: Text -> Context -> Maybe Integer
getValueMaybe = Map.lookup

getValueDefault :: Text -> Integer -> Context -> Integer
getValueDefault k d = Map.findWithDefault d k

getValue :: Text -> Context -> Integer
getValue k = Map.findWithDefault 0 k

compareValueMaybe :: (Text, Integer) -> Context -> Maybe Bool
compareValueMaybe (k,v) = fmap (== v) . getValueMaybe k

compareValueDefault :: (Text, Integer) -> Integer -> Context -> Bool
compareValueDefault (k,v) d = (== v) . getValueDefault k d

compareValue :: (Text, Integer) -> Context -> Bool
compareValue (k, v) = (== v) . getValue k

compareAll :: Context -> Context -> Bool
compareAll c1 c2 = and [ getValue k c1 == getValue k c2 | k <- Map.keys c1 ]

compareNone :: Context -> Context -> Bool
compareNone c1 c2 = and [ getValue k c1 /= getValue k c2 | k <- Map.keys c1 ]

newContext :: Context
newContext = Map.empty
