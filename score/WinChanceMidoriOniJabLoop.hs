{-# LANGUAGE OverloadedStrings #-}

module WinChanceMidoriOniJabLoop
    ( score
    ) where

import Contexts
import Data.Function
import Data.Text (Text)
import Data.Map (Map)
import Control.Monad.State.Lazy (State)

score :: ContextS Double
score = do
    healthA <- fmap (fromIntegral) $ getValue "MidoriHealth"
    healthB <- fmap (fromIntegral) $ getValue "OnimaruHealth"
    chipA <- fmap (fromIntegral) $ getValue "MidoriChip"
    chipB <- fmap (fromIntegral) $ getValue "OnimaruChip"
    superA <- fmap (fromIntegral) $ getValue "MidoriSuper"
    superB <- fmap (fromIntegral) $ getValue "OnimaruSuper"
    
    midoriEmpowered <- fmap (fromIntegral . fromEnum) $ hasValue "MidoriEmpowered"
    oniOki <- fmap (fromIntegral . fromEnum) $ hasValue "OniOki"
    
    case (healthA > 0, healthB > 0) of
         (False, False) -> return $ 0.5
         (False, _    ) -> return $ 0
         (_    , False) -> return $ 1
         _              -> return $ prob healthA healthB chipA chipB superA superB midoriEmpowered oniOki 1 3 1 1
    where
        prob :: Double -> Double -> Double -> Double -> Double -> Double -> Double -> Double -> Double -> Double -> Double -> Double -> Double
        prob healthA healthB chipA chipB superA superB mempower onki tweaktotal tweaksuper tweakmempower tweakonki =
            let
                healthAMod = healthA - chipA*1/3
                healthBMod = healthB - chipB*1/3
                logHealth = log healthAMod - log healthBMod
                superModifier = tweaksuper * (superA-superB) / (healthAMod+healthBMod)
                midoriEmpowered = mempower * tweakmempower
                oniOki = onki * tweakonki
                total = tweaktotal * (logHealth + superModifier + midoriEmpowered - oniOki)
            in
                0.5 + (atan total)/pi
