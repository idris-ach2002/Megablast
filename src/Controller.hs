{- HLINT ignore "Use camelCase" -}

module Controller where

import Graphics.Gloss.Interface.Pure.Game
import Data.Set (Set)
import qualified Data.Set as Set
import Objects
import Engine

---------------------------------------------------------------------------------
-- État global de l'application
-- (séparé du Moteur pur pour garder la logique pure et les effets aux bords)
---------------------------------------------------------------------------------

-- | L'état complet de l'application Gloss.
--   On distingue clairement l'état logique (Moteur) et l'état de l'UI
--   (touches enfoncées, mode pause, demande de redémarrage...).
data AppState = AppState
  { asMoteur       :: Moteur          -- état logique pur
  , asTouchesPres  :: Set Key         -- touches actuellement enfoncées
  , asPause        :: Bool            -- jeu en pause ?
  , asMoteurInitial :: Moteur         -- sauvegarde pour le restart (R)
  , asCadTirJoueuse :: Cadence        -- cadence des projectiles du joueur
  }

-- | Construit un AppState à partir d'un moteur initial.
mkAppState :: Moteur -> Cadence -> AppState
mkAppState m cadTir = AppState
  { asMoteur        = m
  , asTouchesPres   = Set.empty
  , asPause         = False
  , asMoteurInitial = m
  , asCadTirJoueuse = cadTir
  }

---------------------------------------------------------------------------------
-- Correspondance touches → actions pour la joueuse 0 et la joueuse 1
---------------------------------------------------------------------------------

-- | Mapping clavier → (indice joueuse, action).
--   J0 : ZQSD / Espace
--   J1 : Flèches / Entrée
toucheVersAction :: Key -> Maybe (Int, Action)
toucheVersAction (Char 'z')             = Just (1, Deplacer Haut)
toucheVersAction (Char 'q')             = Just (1, Deplacer Gauche)
toucheVersAction (Char 's')             = Just (1, Deplacer Bas)
toucheVersAction (Char 'd')             = Just (1, Deplacer Droite)
toucheVersAction (SpecialKey KeySpace)  = Just (1, Tirer)
-- Joueuse 1 (multijoueur même clavier)
toucheVersAction (SpecialKey KeyUp)     = Just (0, Deplacer Haut)
toucheVersAction (SpecialKey KeyLeft)   = Just (0, Deplacer Gauche)
toucheVersAction (SpecialKey KeyDown)   = Just (0, Deplacer Bas)
toucheVersAction (SpecialKey KeyRight)  = Just (0, Deplacer Droite)
toucheVersAction (SpecialKey KeyEnter)  = Just (0, Tirer)
toucheVersAction _                      = Nothing

---------------------------------------------------------------------------------
-- Handler d'événements Gloss
--
-- Précondition  : l'AppState fourni par Gloss vérifie prop_inv_moteur.
-- Postcondition : l'AppState retourné vérifie prop_inv_moteur.
---------------------------------------------------------------------------------

-- | Gère un événement Gloss (pression/relâchement de touche, etc.).
gererEvenement :: Event -> AppState -> AppState
-- Pause
gererEvenement (EventKey (Char 'p') Down _ _) as =
  as { asPause = not (asPause as) }
-- Restart
gererEvenement (EventKey (Char 'r') Down _ _) as =
  as { asMoteur      = asMoteurInitial as
     , asTouchesPres = Set.empty
     , asPause       = False }
-- Enregistrement de la touche enfoncée
gererEvenement (EventKey k Down _ _) as =
  as { asTouchesPres = Set.insert k (asTouchesPres as) }
-- Relâchement
gererEvenement (EventKey k Up _ _) as =
  as { asTouchesPres = Set.delete k (asTouchesPres as) }
-- Tout autre événement ignoré
gererEvenement _ as = as

-- Précondition portée sur l'état :
prop_pre_gererEvenement :: AppState -> Bool
prop_pre_gererEvenement as = prop_inv_moteur (asMoteur as)

-- Postcondition : l'invariant du moteur est conservé (le moteur n'est pas
-- modifié par un événement clavier, sauf restart qui repose un moteur valide).
prop_post_gererEvenement :: AppState -> AppState -> Bool
prop_post_gererEvenement _ as' = prop_inv_moteur (asMoteur as')

---------------------------------------------------------------------------------
-- Application des commandes issues des touches enfoncées
--
-- Appelée à chaque tick pour traduire l'ensemble des touches maintenues
-- en actions sur le moteur logique.
---------------------------------------------------------------------------------

-- | Pour chaque touche enfoncée, applique l'action correspondante au moteur.
appliquerCommandes :: AppState -> Moteur
appliquerCommandes as =
  let touches = Set.toList (asTouchesPres as)
      actions  = [ (i, a) | k <- touches
                           , Just (i, a) <- [toucheVersAction k]
                           , i < length (mJoueuses (asMoteur as)) ]
      cadTir   = asCadTirJoueuse as
  in foldr (\(i, a) m -> fst $ appliquerCommande i a cadTir m)
           (asMoteur as)
           actions

-- | Postcondition : appliquerCommandes préserve l'invariant.
prop_post_appliquerCommandes :: AppState -> Bool
prop_post_appliquerCommandes as =
  prop_inv_moteur (appliquerCommandes as)

---------------------------------------------------------------------------------
-- Boucle de simulation (step)
--
-- Gloss appelle cette fonction à chaque frame avec le temps écoulé (Float).
-- On convertit le temps en nombre de tours discrets.
---------------------------------------------------------------------------------

-- | Nombre de tours de jeu par seconde (indépendant du framerate).
toursParSeconde :: Float
toursParSeconde = 30

-- | Accumulateur fractionnaire (en secondes) reporté d'une frame à l'autre.
--   On l'intègre dans AppState pour éviter toute dérive.
--
-- Note : pour rester dans le style pur, on utilise un champ dédié.
--        Les effets (IO) restent dans Main.
data AppStateFull = AppStateFull
  { asfBase :: AppState
  , asfAccu :: Float     -- temps accumulé depuis le dernier tour entier
  }

mkAppStateFull :: AppState -> AppStateFull
mkAppStateFull as = AppStateFull as 0

-- | Met à jour l'état à chaque frame Gloss.
--
-- Précondition  : prop_inv_moteur (asMoteur (asfBase s))
-- Postcondition : prop_inv_moteur (asMoteur (asfBase s'))
--              && mTour (asMoteur (asfBase s')) >= mTour (asMoteur (asfBase s))
simulerStep :: Float -> AppStateFull -> AppStateFull
simulerStep dt asf
  | asPause (asfBase asf) = asf   -- jeu en pause : aucune évolution
  | not (prop_partie_en_cours (asMoteur (asfBase asf))) = asf  -- game over
  | otherwise =
      let accu'   = asfAccu asf + dt
          nTours  = floor (accu' * toursParSeconde) :: Int
          accu''  = accu' - fromIntegral nTours / toursParSeconde
          -- On applique d'abord les commandes clavier, puis les tours moteur
          m0      = appliquerCommandes (asfBase asf)
          m'      = iterate finDeTourMoteur m0 !! nTours
      in asf { asfBase = (asfBase asf) { asMoteur = m' }
             , asfAccu = accu'' }

prop_pre_simulerStep :: AppStateFull -> Bool
prop_pre_simulerStep = prop_inv_moteur . asMoteur . asfBase

prop_post_simulerStep :: Float -> AppStateFull -> AppStateFull -> Bool
prop_post_simulerStep _ _ asf' =
  prop_inv_moteur (asMoteur (asfBase asf'))

---------------------------------------------------------------------------------
-- Wrapper EventHandler pour AppStateFull (utilisé dans Main)
---------------------------------------------------------------------------------

gererEvenementFull :: Event -> AppStateFull -> AppStateFull
gererEvenementFull e asf =
  asf { asfBase = gererEvenement e (asfBase asf) }
