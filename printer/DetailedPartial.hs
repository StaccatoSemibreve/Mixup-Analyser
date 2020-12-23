{-# LANGUAGE OverloadedStrings #-}

module DetailedPartial
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
import Formatting
import Formatting.Formatters

printer :: TreeGame -> Text
printer tree = pack . drawTree . fmap (unpack . prettyshownode) . pruneTree $ tree
    where
        prettyshownode :: (Context, (Text, a), (Text, a), GameComplex) -> Text
        prettyshownode (c, ("None1", _), ("None2", _), g) = prettyshowgame g c
        prettyshownode (c, (o1, _), ("None2", _), g) = sformat (stext % ": " % stext) o1 (prettyshowgame g c)
        prettyshownode (c, ("None1", _), (o2, _), g) = sformat (stext % ": " % stext) o2 (prettyshowgame g c)
        prettyshownode (c, (o1, _), (o2, _), g) = sformat (stext % " + " % stext % ": " % stext) o1 o2 (prettyshowgame g c)
        
        prettyshowgame :: GameComplex -> Context -> Text
        prettyshowgame (GameComplex "" _ _ gdata (Just gout)) c = sformat ("\n" % stext % stext) (prettyshowcontext c) (prettyshowres gout)
        prettyshowgame (GameComplex gname "" "" gdata (Just gout)) c = sformat (stext % "\n" % stext % "\n" % stext) gname (prettyshowcontext c) (prettyshowres gout)
        prettyshowgame (GameComplex gname attname defname gdata (Just gout)) c = sformat (stext % "\n" % stext % "\n (" % stext % " vs " % stext % ")" % stext) gname (prettyshowcontext c) attname defname (prettyshowres gout)
        
        prettyshowcontext :: Context -> Text
        prettyshowcontext c = foldr prettyshowkeyval (prettyshowkeyvalfirst . head . M.toList $ c) . tail . M.toList $ c
        
        prettyshowkeyvalfirst :: (Text, Integer) -> Text
        prettyshowkeyvalfirst (k, v) = sformat ("Context: " % stext % ": " % int) k v
        prettyshowkeyval :: (Text, Integer) -> Text -> Text
        prettyshowkeyval (k, v) acc = sformat (stext % ", " % stext % ": " % int) acc k v
        
        prettyshowres :: ResultComplex -> Text
        prettyshowres (ResultComplex ev sd [_] [_]) = sformat ("\n EV: " % fixed 3 % "\n SD: " % fixed 3) ev sd
        prettyshowres (ResultComplex ev sd [_] defs) = sformat ("\n EV: " % fixed 3 % "\n SD: " % fixed 3 % "\n Defender Options: " % stext) ev sd (prettyshowouts defs)
        prettyshowres (ResultComplex ev sd atts [_]) = sformat ("\n EV: " % fixed 3 % "\n SD: " % fixed 3 % "\n Attacker Options: " % stext) ev sd (prettyshowouts atts)
        prettyshowres (ResultComplex ev sd atts defs) = sformat ("\n EV: " % fixed 3 % "\n SD: " % fixed 3 % "\n Attacker Options: " % stext % "\n Defender Options: " % stext) ev sd (prettyshowouts atts) (prettyshowouts defs)
        
        prettyshowouts :: [(Text, Double)] -> Text
        prettyshowouts outs = (\l -> foldl (\a b -> sformat (stext % " - " % stext) a b) (head l) (tail l)) . map prettyshowpair $ outs
        prettyshowpair :: (Text, Double) -> Text
        prettyshowpair (opt, val) = sformat (stext % ": " % fixed 0 % "%") opt (val*100)

pruneTree :: TreeGame -> TreeGame
pruneTree = unfoldTree pruneTreeUnfolder
    where
        pruneTreeUnfolder :: TreeGame -> ((Context, (Text, Maybe Double), (Text, Maybe Double), GameComplex), [TreeGame])
        pruneTreeUnfolder x = (foldTree pruneTreeFolder x, filter (nonzero . foldTree pruneTreeFolder) . subForest $ x)
        
        pruneTreeFolder :: (Context, (Text, Maybe Double), (Text, Maybe Double), GameComplex) -> [(Context, (Text, Maybe Double), (Text, Maybe Double), GameComplex)] -> (Context, (Text, Maybe Double), (Text, Maybe Double), GameComplex)
        pruneTreeFolder a _ = a
        
        nonzero :: (a, (Text, b), (Text, b), GameComplex) -> Bool
        nonzero (_, (att,_),(def,_),GameComplex _ _ _ _ (Just (ResultComplex _ _ atts defs))) = and [(/= 0) . fromMaybe 0 . lookup att $ atts, (/= 0) . fromMaybe 0 . lookup def $ defs]
