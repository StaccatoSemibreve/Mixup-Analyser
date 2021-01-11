{-# LANGUAGE OverloadedStrings #-}

-- the helper functions used by Custom so that it's not also full of things that should never be altered
module Contexts
    ( setValue, sets, addValue, adds, addset
    , hasValue, removeValue, getValue, getValueDefault, getValueMaybe
    , compareValue, compareValueDefault, compareValueMaybe, compareAll, compareNone
    , newContext
    , Context, ContextS
    ) where

import Data.List
import Data.Function
import Data.Maybe
import Data.Text (Text)
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad.State.Lazy

type Context = Map Text Integer
type ContextS = State Context

setValue :: Text -> Integer -> ContextS ()
setValue k v = modify $ Map.insert k v

sets :: Context -> ContextS ()
sets c = modify $ Map.union c

addValue :: Text -> Integer -> ContextS ()
addValue k v = modify $ Map.insertWith (+) k v

adds :: Context -> ContextS ()
adds c = do
    context <- get
    put $ Map.unionWith (+) c context

addset :: Context -> Context -> ContextS ()
addset s a = do
    adds a
    sets s

removeValue :: Text -> ContextS ()
removeValue k = modify $ Map.delete k

hasValue :: Text -> ContextS Bool
hasValue k = gets $ Map.member k

getValueMaybe :: Text -> ContextS (Maybe Integer)
getValueMaybe k = gets $ Map.lookup k

getValueDefault :: Text -> Integer -> ContextS Integer
getValueDefault k d = gets $ Map.findWithDefault d k

getValue :: Text -> ContextS Integer
getValue k = gets $ Map.findWithDefault 0 k

compareValueMaybe :: Text -> Integer -> ContextS (Maybe Bool)
compareValueMaybe k v = do
    v2 <- getValueMaybe k
    return $ fmap (==v) v2

compareValueDefault :: Text -> Integer -> Integer -> ContextS Bool
compareValueDefault k v d = do
    v2 <- getValueDefault k d
    return $ v == v2

compareValue :: Text -> Integer -> ContextS Bool
compareValue k v = do
    v2 <- getValue k
    return $ v == v2

compareAll :: Context -> ContextS Bool
compareAll c1 = do
    c2 <- get
    return $ and [ Map.findWithDefault 0 k c1 == Map.findWithDefault 0 k c2 | k <- Map.keys c1 ]

compareNone :: Context -> ContextS Bool
compareNone c1 = do
    c2 <- get
    return $ and [ Map.findWithDefault 0 k c1 /= Map.findWithDefault 0 k c2 | k <- Map.keys c1 ]

newContext :: Context
newContext = Map.empty
