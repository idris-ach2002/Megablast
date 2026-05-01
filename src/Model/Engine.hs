{- HLINT ignore "Use camelCase" -}

-- | Façade publique du moteur.
--
--   Les responsabilités internes sont séparées dans Model.Engine.*,
--   mais les autres modules du projet peuvent continuer à importer
--   simplement Model.Engine comme avant.
{- HLINT ignore "Use camelCase" #-}

module Model.Engine
  ( module Model.Engine.Types
  , module Model.Meteore
  , module Model.Engine.Murs
  , module Model.Engine.Collisions
  , module Model.Engine.EnnemiSpawn
  , module Model.Engine.Step
  , module Model.Engine.Commands
  , module Model.Engine.Examples
  , module Model.Engine.Properties
  , module Model.Engine.ListUtils
  , module Model.Engine.EnnemiAI
  , module Model.Engine.EnnemiCollisions
  , module Model.Score
  , module Model.Engine.GameConfig
  , module Model.Engine.DynamicSpawn
  ) where

import Model.Engine.Collisions
import Model.Engine.Commands
import Model.Engine.EnnemiSpawn
import Model.Engine.Examples
import Model.Engine.ListUtils
import Model.Meteore
import Model.Engine.Murs
import Model.Engine.Properties
import Model.Engine.Step
import Model.Engine.Types
import Model.Engine.EnnemiAI
import Model.Engine.EnnemiCollisions
import Model.Score
import Model.Engine.GameConfig
import Model.Engine.DynamicSpawn