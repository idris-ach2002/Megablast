{-# LANGUAGE OverloadedStrings #-}

module Model.Engine.Examples where

import Data.Text (Text)
import Model.Engine.GameConfig
import Model.Engine.Types

-- | Ancien point d'entrée conservé pour compatibilité.
--   Le vrai moteur initial passe maintenant par ConfigPartie.
exempleMoteur :: Either Text Moteur
exempleMoteur =
  mkMoteurPartie configPartieDefaut