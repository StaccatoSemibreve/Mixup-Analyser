{-# LANGUAGE OverloadedStrings #-}

module Summary
    ( printer
    ) where

import GameSolve
import ParseData
import Evaluate
import Contexts
import Score

import Data.Maybe
import Data.Text (Text, pack, unpack, append)
import qualified Data.Text as T
import Data.Tree
import Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as M
import Formatting
import Formatting.Formatters
import Formatting.Combinators
import Control.Monad.State.Lazy (evalState)
import Control.Monad.Reader (ReaderT)
import Control.Monad.Identity

printer :: Printer
printer (Node tgi _) = do
    scorea <- scoresatt startcontext
    scored <- scoresdef startcontext
    return $ sformat ("Initial scores were this in neutral:\nAttacker: " % commaSpaceSep (fixed 3) % "\nDefender: " % commaSpaceSep (fixed 3) % "\n\n" % stext) scorea scored (prettyshownode tgi)
    where
        startcontext = tgiContext tgi
        
        prettyshownode :: TreeGameItem -> Text
        prettyshownode tgi = 
            let
                att = fst . tgiAtt $ tgi
                def = fst . tgiDef $ tgi
                aEVs = tgiAttEVs tgi
                dEVs = tgiDefEVs tgi
                aSDs = tgiAttSDs tgi
                dSDs = tgiDefSDs tgi
                c = tgiContext tgi
                g = tgiGame tgi
                
                evT = sformat ("Attacker EVs: " % commaSpaceSep (fixed 3) % "\nDefender EVs: " % commaSpaceSep (fixed 3)) aEVs dEVs
                sdT = case (aSDs, dSDs) of
                           ([],[]) -> ""
                           ([], _) -> sformat ("\nDefender SDs: " % commaSpaceSep (fixed 3)) dSDs
                           (_ ,[]) -> sformat ("\nAttacker SDs: " % commaSpaceSep (fixed 3)) aSDs
                           (_ , _) -> sformat ("\nAttacker SDs: " % commaSpaceSep (fixed 3) % "\nDefender SDs: " % commaSpaceSep (fixed 3)) aSDs dSDs
            in
                case (att, def) of
                     ("None1", "None2") -> sformat (stext % stext % "\n" % stext) evT sdT (prettyshowgame g c)
                     (_      , "None2") -> sformat (stext % ":\n" % stext % stext % "\n" % stext) att evT sdT (prettyshowgame g c)
                     ("None1", _      ) -> sformat (stext % ":\n" % stext % stext % "\n" % stext) def evT sdT (prettyshowgame g c)
                     _                  -> sformat (stext % " + " % stext % ":\n" % stext % stext % "\n" % stext) att def evT sdT (prettyshowgame g c)
        
        prettyshowgame :: Game -> Context -> Text
        prettyshowgame (Game "" _ _ gdata (Just gout)) c                = sformat ("\n" % stext % stext) (prettyshowcontext c) (prettyshowres gout)
        prettyshowgame (Game gname "" "" gdata (Just gout)) c           = sformat ("\n" % stext % "\n" % stext % "\n" % stext) gname (prettyshowcontext c) (prettyshowres gout)
        prettyshowgame (Game gname attname defname gdata (Just gout)) c = sformat ("\n" % stext % "\n" % stext % "\n (" % stext % " vs " % stext % ")" % stext) gname (prettyshowcontext c) attname defname (prettyshowres gout)
        
        prettyshowcontext :: Context -> Text
        prettyshowcontext c = foldr prettyshowkeyval (prettyshowkeyvalfirst . head . M.toList $ c) . tail . M.toList $ c
        
        prettyshowkeyvalfirst :: (Text, Int) -> Text
        prettyshowkeyvalfirst (k, v) = sformat (" Context: " % stext % ": " % int) k v
        prettyshowkeyval :: (Text, Int) -> Text -> Text
        prettyshowkeyval (k, v) acc = sformat (stext % ", " % stext % ": " % int) acc k v
        
        prettyshowres :: Result -> Text
        prettyshowres (Result evc evr sdc sdr [_] [_])      = ""
        prettyshowres (Result evc evr sdc sdr [_] defs)     = sformat ("\n Defender Options: " % stext) (prettyshowouts defs)
        prettyshowres (Result evc evr sdc sdr atts [_])     = sformat ("\n Attacker Options: " % stext) (prettyshowouts atts)
        prettyshowres (Result evc evr sdc sdr atts defs)    = sformat ("\n Attacker Options: " % stext % "\n Defender Options: " % stext) (prettyshowouts atts) (prettyshowouts defs)
        
        prettyshowouts :: [(Text, Double)] -> Text
        prettyshowouts outs = (\l -> foldl (\a b -> sformat (stext % " - " % stext) a b) (head l) (tail l)) . map prettyshowpair $ outs
        prettyshowpair :: (Text, Double) -> Text
        prettyshowpair (opt, val) = sformat (stext % ": " % fixed 0 % "%") opt (val*100)
