{-# LANGUAGE OverloadedStrings #-}

module WinChance
    ( score
    ) where

import Contexts
import Data.Function
import Data.Text (Text)
import Data.Map (Map)
import Control.Monad.State.Lazy (State)

score :: ContextS Double
score = do
    healthA <- fmap (fromIntegral) $ getValue "AHealth"
    healthB <- fmap (fromIntegral) $ getValue "BHealth"
    chipA <- fmap (fromIntegral) $ getValue "AChip"
    chipB <- fmap (fromIntegral) $ getValue "BChip"
    superA <- fmap (fromIntegral) $ getValue "ASuper"
    superB <- fmap (fromIntegral) $ getValue "BSuper"
    
    case (healthA > 0, healthB > 0) of
         (False, False) -> return $ 0.5
         (False, _    ) -> return $ 0
         (_    , False) -> return $ 1
         _              -> return $ prob healthA healthB chipA chipB superA superB 1 3
    where
        prob :: Double -> Double -> Double -> Double -> Double -> Double -> Double -> Double -> Double
        prob healthA healthB chipA chipB superA superB tweaktotal tweaksuper =
            let
                healthAMod = healthA - chipA*1/3
                healthBMod = healthB - chipB*1/3
                logHealth = log healthAMod - log healthBMod
                superModifier = tweaksuper * (superA-superB) / (healthAMod+healthBMod)
                total = tweaktotal * (logHealth + superModifier)
            in
                0.5 + (atan total)/pi
