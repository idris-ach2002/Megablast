{-# HLINT ignore "Use camelCase" #-}

module Controller.Controller where

import Graphics.Gloss.Interface.Pure.Game
import Data.Set (Set)
import qualified Data.Set as Set
import Model.Objects
import Model.Engine

---------------------------------------------------------------------------------
-- État global de l'application
---------------------------------------------------------------------------------

data AppState = AppState
  { asMoteur         :: Moteur
  , asTouchesPres    :: Set Key
  , asPause          :: Bool
  , asMoteurInitial  :: Moteur
  , asCadTirJoueuse  :: Cadence
  }

mkAppState :: Moteur -> Cadence -> AppState
mkAppState m cadTir = AppState
  { asMoteur         = m
  , asTouchesPres    = Set.empty
  , asPause          = False
  , asMoteurInitial  = m
  , asCadTirJoueuse  = cadTir
  }

---------------------------------------------------------------------------------
-- Correspondance touches → actions
---------------------------------------------------------------------------------

toucheVersAction :: Key -> Maybe (Int, Action)
toucheVersAction (Char 'z')             = Just (1, Deplacer Haut)
toucheVersAction (Char 'q')             = Just (1, Deplacer Gauche)
toucheVersAction (Char 's')             = Just (1, Deplacer Bas)
toucheVersAction (Char 'd')             = Just (1, Deplacer Droite)
toucheVersAction (SpecialKey KeySpace)  = Just (1, Tirer)
toucheVersAction (SpecialKey KeyUp)     = Just (0, Deplacer Haut)
toucheVersAction (SpecialKey KeyLeft)   = Just (0, Deplacer Gauche)
toucheVersAction (SpecialKey KeyDown)   = Just (0, Deplacer Bas)
toucheVersAction (SpecialKey KeyRight)  = Just (0, Deplacer Droite)
toucheVersAction (SpecialKey KeyEnter)  = Just (0, Tirer)
toucheVersAction _                      = Nothing

---------------------------------------------------------------------------------
-- Gestion des événements Gloss
---------------------------------------------------------------------------------

gererEvenement :: Event -> AppState -> AppState
gererEvenement (EventKey (Char 'p') Down _ _) as =
  as { asPause = not (asPause as) }
gererEvenement (EventKey (Char 'r') Down _ _) as =
  as { asMoteur      = asMoteurInitial as
     , asTouchesPres = Set.empty
     , asPause       = False }
gererEvenement (EventKey k Down _ _) as =
  as { asTouchesPres = Set.insert k (asTouchesPres as) }
gererEvenement (EventKey k Up _ _) as =
  as { asTouchesPres = Set.delete k (asTouchesPres as) }
gererEvenement _ as = as

prop_pre_gererEvenement :: AppState -> Bool
prop_pre_gererEvenement as = prop_inv_moteur (asMoteur as)

prop_post_gererEvenement :: AppState -> AppState -> Bool
prop_post_gererEvenement _ as' = prop_inv_moteur (asMoteur as')

---------------------------------------------------------------------------------
-- Application des commandes clavier
---------------------------------------------------------------------------------

actionsDepuisTouches :: AppState -> [(Int, Action)]
actionsDepuisTouches as =
  [ (i, a)
  | k <- Set.toList (asTouchesPres as)
  , Just (i, a) <- [toucheVersAction k]
  , i < length (mJoueuses (asMoteur as))
  ]

appliquerActions :: [(Int, Action)] -> Cadence -> Moteur -> Moteur
appliquerActions actions cadTir moteur =
  foldr
    (\(i, a) m -> fst (appliquerCommande i a cadTir m))
    moteur
    actions

appliquerCommandes :: AppState -> Moteur
appliquerCommandes as =
  appliquerActions
    (actionsDepuisTouches as)
    (asCadTirJoueuse as)
    (asMoteur as)

prop_post_appliquerCommandes :: AppState -> Bool
prop_post_appliquerCommandes as =
  prop_inv_moteur (appliquerCommandes as)

---------------------------------------------------------------------------------
-- Boucle de simulation
---------------------------------------------------------------------------------

toursParSeconde :: Float
toursParSeconde = 120

data AppStateFull = AppStateFull
  { asfBase :: AppState
  , asfAccu :: Float
  }

mkAppStateFull :: AppState -> AppStateFull
mkAppStateFull as = AppStateFull as 0

-- | Applique n tours moteur de manière sûre en rejouant les actions courantes à
-- chaque tour. Si un tour échoue, on s'arrête et on renvoie l'erreur.
appliquerNToursAvecCommandesEither :: Int -> [(Int, Action)] -> Cadence -> Moteur -> Either String Moteur
appliquerNToursAvecCommandesEither n actions cadTir m
  | n <= 0 = Right m
  | otherwise =
      let m0 = appliquerActions actions cadTir m
      in case finDeTourMoteurEither m0 of
           Left err -> Left (show err)
           Right m' -> appliquerNToursAvecCommandesEither (n - 1) actions cadTir m'

simulerStep :: Float -> AppStateFull -> AppStateFull
simulerStep dt asf
  | asPause (asfBase asf) = asf
  | not (prop_partie_en_cours (asMoteur (asfBase asf))) = asf
  | otherwise =
      let accu'   = asfAccu asf + dt
          nTours  = floor (accu' * toursParSeconde) :: Int
          accu''  = accu' - fromIntegral nTours / toursParSeconde
          actions = actionsDepuisTouches (asfBase asf)
          cadTir  = asCadTirJoueuse (asfBase asf)
          mInitial = asMoteur (asfBase asf)
          m' = case appliquerNToursAvecCommandesEither nTours actions cadTir mInitial of
                 Right mOk -> mOk
                 Left _    -> mInitial
      in asf { asfBase = (asfBase asf) { asMoteur = m' }
             , asfAccu = accu''
             }

prop_pre_simulerStep :: AppStateFull -> Bool
prop_pre_simulerStep = prop_inv_moteur . asMoteur . asfBase

prop_post_simulerStep :: Float -> AppStateFull -> AppStateFull -> Bool
prop_post_simulerStep _ _ asf' =
  prop_inv_moteur (asMoteur (asfBase asf'))

---------------------------------------------------------------------------------
-- Wrapper EventHandler pour AppStateFull
---------------------------------------------------------------------------------

gererEvenementFull :: Event -> AppStateFull -> AppStateFull
gererEvenementFull e asf =
  asf { asfBase = gererEvenement e (asfBase asf) }
