{-# LANGUAGE OverloadedStrings #-}

module Detailed
    ( printer
    ) where

import Game
import Parse
import Evaluate
import Contexts

import Data.Maybe
import Data.Text (Text, pack, unpack, append)
import qualified Data.Text as T
import Data.Tree
import Data.Map (Map)
import qualified Data.Map as M

printer :: TreeGame -> String
printer tree = drawTree . fmap (unpack . prettyshownode) $ tree
    where
        prettyshownode :: (Context, (Text, a), (Text, a), GameComplex) -> Text
        prettyshownode (c, ("None1", _), ("None2", _), g) = prettyshowgame g c
        prettyshownode (c, (o1, _), ("None2", _), g) = o1 `append` ": " `append` (prettyshowgame g c)
        prettyshownode (c, ("None1", _), (o2, _), g) = o2 `append` ": " `append` (prettyshowgame g c)
        prettyshownode (c, (o1, _), (o2, _), g) = o1 `append` " + " `append` o2 `append` ": " `append` (prettyshowgame g c)
        
        prettyshowgame :: GameComplex -> Context -> Text
        prettyshowgame (GameComplex "" _ _ gdata (Just gout)) c = "\n" `append` (prettyshowcontext c) `append` prettyshowres gout
        prettyshowgame (GameComplex gname "" "" gdata (Just gout)) c = gname `append` "\n" `append` (prettyshowcontext c) `append` "\n" `append` (prettyshowres gout)
        prettyshowgame (GameComplex gname attname defname gdata (Just gout)) c = gname `append` "\n" `append` (prettyshowcontext c) `append` "\n" `append` " (" `append` attname `append` " vs " `append` defname `append` ")" `append` (prettyshowres gout)
        
        prettyshowcontext :: Context -> Text
        prettyshowcontext c = foldr prettyshowkeyval (prettyshowkeyvalfirst . head . M.toList $ c) . tail . M.toList $ c
        
        prettyshowkeyvalfirst :: (Text, Integer) -> Text
        prettyshowkeyvalfirst (k, v) = " Context: " `append` k `append` ": " `append` (pack . show $ v)
        prettyshowkeyval :: (Text, Integer) -> Text -> Text
        prettyshowkeyval (k, v) acc = acc `append` ", " `append` k `append` ": " `append` (pack . show $ v)
        
        prettyshowres :: ResultComplex -> Text
        prettyshowres (ResultComplex ev sd [_] [_]) = "\n EV: " `append` (pack . show $ ev) `append` "\n SD: " `append` (pack . show $ sd)
        prettyshowres (ResultComplex ev sd [_] defs) = "\n EV: " `append` (pack . show $ ev) `append` "\n SD: " `append` (pack . show $ sd) `append` "\n Defender Options: " `append` (prettyshowouts defs)
        prettyshowres (ResultComplex ev sd atts [_]) = "\n EV: " `append` (pack . show $ ev) `append` "\n SD: " `append` (pack . show $ sd) `append` "\n Attacker Options: " `append` (prettyshowouts atts)
        prettyshowres (ResultComplex ev sd atts defs) = "\n EV: " `append` (pack . show $ ev) `append` "\n SD: " `append` (pack . show $ sd) `append` "\n Attacker Options: " `append` (prettyshowouts atts) `append` "\n Defender Options: " `append` (prettyshowouts defs)
        
        prettyshowouts :: [(Text, Double)] -> Text
        prettyshowouts outs = (\l -> foldl (\a b -> a `append` " - " `append` b) (head l) (tail l)) . map prettyshowpair $ outs
        prettyshowpair :: (Text, Double) -> Text
        prettyshowpair (opt, val) = opt `append` ": " `append` (pack . show $ val)
