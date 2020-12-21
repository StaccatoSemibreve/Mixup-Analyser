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

setValue :: (Text, v) -> Map Text v -> Map Text v
setValue (k,v) c = Map.insert k v c

sets :: Map Text v -> Map Text v -> Map Text v
sets = Map.union

addValue :: Num v => (Text, v) -> Map Text v -> Map Text v
addValue (k,v) c = Map.insertWith (+) k v c

adds :: Num v => Map Text v -> Map Text v -> Map Text v
adds = Map.unionWith (+)

addset :: Num v => Map Text v -> Map Text v -> Map Text v -> Map Text v
addset s a c = adds a . sets s $ c

getValueMaybe :: Text -> Map Text v -> Maybe v
getValueMaybe = Map.lookup

getValueDefault :: Text -> v -> Map Text v -> v
getValueDefault k d = Map.findWithDefault d k

getValue :: Num v => Text -> Map Text v -> v
getValue k = Map.findWithDefault 0 k

compareValueMaybe :: Eq v => (Text, v) -> Map Text v -> Maybe Bool
compareValueMaybe (k,v) = fmap (== v) . getValueMaybe k

compareValueDefault :: Eq v => (Text, v) -> v -> Map Text v -> Bool
compareValueDefault (k,v) d = (== v) . getValueDefault k d

compareValue :: (Num v, Eq v) => (Text, v) -> Map Text v -> Bool
compareValue (k, v) = (== v) . getValue k

compareAll :: Eq v => Map Text v -> Map Text v -> Bool
compareAll c = and . Map.elems . Map.intersectionWith (==) c

compareNone :: Eq v => Map Text v -> Map Text v -> Bool
compareNone c = not . or . Map.elems . Map.intersectionWith (==) c

newContext :: Context
newContext = Map.empty
