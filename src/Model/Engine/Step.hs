{-# LANGUAGE OverloadedStrings #-}

module Model.Engine.Step where

import Data.List (partition)
import Data.Text (Text)
import Model.Engine.Collisions
import Model.Engine.EnnemiSpawn
import Model.Engine.EnnemiAI
import Model.Engine.ListUtils
import Model.Engine.Murs
import Model.Engine.Types
import Model.Hitbox
import Model.Meteore
import Model.Objects
import System.Random (StdGen, random)

-- | Application d'un événement du script.
appliquerEvenement :: Evenement -> Moteur -> Moteur
appliquerEvenement (AppEnnemi e)      m = m { mEnnemis     = mEnnemis m ++ [e] }
appliquerEvenement (AppObstacle o)    m = m { mObstacles   = mObstacles m ++ [o] }
appliquerEvenement (AppProjectile p)  m = m { mProjectiles = mProjectiles m ++ [p] }
appliquerEvenement (AppMeteore mt)    m = m { mMeteores    = mMeteores m ++ [mt] }
appliquerEvenement (DisparEnnemi i)   m = m { mEnnemis     = removeAt i (mEnnemis m) }
appliquerEvenement (DisparObstacle i) m = m { mObstacles   = removeAt i (mObstacles m) }
appliquerEvenement (DisparMeteore i)  m = m { mMeteores    = removeAt i (mMeteores m) }

prop_pre_finDeTourMoteur :: Moteur -> Bool
prop_pre_finDeTourMoteur m =
  prop_inv_moteur m && prop_partie_en_cours m

prop_post_finDeTourMoteur :: Moteur -> Moteur -> Bool
prop_post_finDeTourMoteur m m' =
     prop_inv_moteur m'
  && mTour m' == mTour m + 1
  && length (mJoueuses m') <= length (mJoueuses m)
  && all joueuseEncoreEnJeu (mJoueuses m')
  && all prop_inv_projectile (mProjectiles m')

-- | Version sûre de la fin de tour. Elle rend explicite l'échec par `Either`
--   au lieu de supposer implicitement que l'invariant est déjà respecté.
finDeTourMoteurEither :: Moteur -> Either Text Moteur
finDeTourMoteurEither m
  | not (prop_inv_moteur m) = Left "finDeTourMoteur: invariant moteur violé"
  | otherwise =
      let tourActuel =
            mTour m

          (evtsNow, evtsFutur) =
            partition (\ep -> epTour ep <= tourActuel) (mScript m)

          m0 =
            foldr (appliquerEvenement . epEvenement) m evtsNow

          m1 =
            m0 { mScript = evtsFutur }

          -- Nouvelle étape : apparition automatique des ennemis.
          mSpawn =
            genererEnnemiSiBesoin m1

          (scrollNow, cadScroll') =
            tickCadence (mCadScroll mSpawn)

          obsDefiles =
            if scrollNow
              then map defileObstacle (mObstacles mSpawn)
              else mObstacles mSpawn

          obsValides =
            filter (not . horsEcranMoteur . obsHitbox) obsDefiles

          murs' =
            if scrollNow
              then defileMurs (mMurs mSpawn)
              else mMurs mSpawn

          meteoresTour =
            map finDeTourMeteore (mMeteores mSpawn)

          meteoresValides =
            filter (not . horsEcranMoteur . mtHitbox) meteoresTour

          projs' =
            map finDeTourProjectile (mProjectiles mSpawn)

          rng =
            mRng mSpawn

          (graineEnnemis, rngNext) =
            random rng :: (Int, StdGen)

          (ennsTour, newProjs) =
             unzip $
                map
                  (finDeTourEnnemiIntelligent graineEnnemis (mJoueuses mSpawn))
                  (mEnnemis mSpawn)
                  
          ennsValides =
            filter (not . horsEcranMoteur . eHitbox) ennsTour

          projsEnnemis =
            [ p | Just p <- newProjs ]

          tousProjs =
            projs' ++ projsEnnemis

          projsValides =
            filter (not . horsEcranMoteur . prHitbox) tousProjs

          m2 =
            resoudreCollisions
              (mSpawn { mObstacles   = obsValides
                      , mProjectiles = projsValides
                      , mEnnemis     = ennsValides
                      , mMeteores    = meteoresValides
                      , mMurs        = murs'
                      , mCadScroll   = cadScroll'
                      })

          m3 =
            m2 { mTour = tourActuel + 1
               , mRng  = rngNext
               }

      in Right m3

finDeTourMoteur :: Moteur -> Moteur
finDeTourMoteur m =
  case finDeTourMoteurEither m of
    Right m' -> m'
    Left _   -> m

-- | Détermine si une hitbox est hors de l'écran logique du moteur.
horsEcranHitbox :: Int -> Int -> Int -> Hitbox -> Bool
horsEcranHitbox largeur hauteur marge (Point x y) =
  x < (-marge) || x > largeur + marge || y < (-marge) || y > hauteur + marge

horsEcranHitbox largeur hauteur marge (Disque xc yc r) =
  xc + r < (-marge) || xc - r > largeur + marge
  || yc + r < (-marge) || yc - r > hauteur + marge

horsEcranHitbox largeur hauteur marge (Model.Hitbox.Rectangle x y w h) =
  x + w < (-marge) || x > largeur + marge
  || y + h < (-marge) || y > hauteur + marge

horsEcranHitbox largeur hauteur marge (Composee hs) =
  all (horsEcranHitbox largeur hauteur marge) hs

horsEcranHitbox _ _ _ _ =
  False

horsEcranMoteur :: Hitbox -> Bool
horsEcranMoteur =
  horsEcranHitbox largeurZoneJeu hauteurZoneJeu margeHorsEcran