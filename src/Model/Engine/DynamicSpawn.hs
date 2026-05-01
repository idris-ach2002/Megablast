{-# LANGUAGE OverloadedStrings #-}

module Model.Engine.DynamicSpawn where

import Model.Engine.Types
import Model.Hitbox
import Model.Meteore
import Model.Objects
import System.Random (randomR)

---------------------------------------------------------------------------------
-- Génération dynamique bornée
---------------------------------------------------------------------------------

maxEnnemisDynamiques :: Int
maxEnnemisDynamiques = 5

maxMeteoresDynamiques :: Int
maxMeteoresDynamiques = 4

maxObstaclesDynamiques :: Int
maxObstaclesDynamiques = 7

cadenceTirEnnemiMin :: Int
cadenceTirEnnemiMin = 120

cadenceTirEnnemiMax :: Int
cadenceTirEnnemiMax = 220

-- | Point d'entrée unique de génération.
--   Le moteur ne conserve que les objets actifs.
--   Si une catégorie est sous son quota et que la période tombe juste,
--   on crée un nouvel objet avec une position pseudo-aléatoire.
genererObjetsDynamiques :: Moteur -> Moteur
genererObjetsDynamiques =
  genererObstacleSiBesoin
  . genererMeteoreSiBesoin
  . genererEnnemiSiBesoinDyn

---------------------------------------------------------------------------------
-- Difficulté progressive
---------------------------------------------------------------------------------

periodeEnnemi :: Int -> Int
periodeEnnemi tour
  | tour < 1500 = 260
  | tour < 3500 = 210
  | tour < 6000 = 170
  | otherwise   = 130

periodeMeteore :: Int -> Int
periodeMeteore tour
  | tour < 1500 = 240
  | tour < 3500 = 190
  | tour < 6000 = 150
  | otherwise   = 110

periodeObstacle :: Int -> Int
periodeObstacle tour
  | tour < 1500 = 360
  | tour < 3500 = 300
  | tour < 6000 = 240
  | otherwise   = 180

doitGenerer :: Int -> Int -> Bool
doitGenerer tour periode =
  tour > 0 && tour `mod` periode == 0

---------------------------------------------------------------------------------
-- Tirage pseudo-aléatoire
---------------------------------------------------------------------------------

tirerInt :: (Int, Int) -> Moteur -> (Int, Moteur)
tirerInt bornes m =
  let (x, rng') = randomR bornes (mRng m)
  in (x, m { mRng = rng' })

---------------------------------------------------------------------------------
-- Ennemis
---------------------------------------------------------------------------------

genererEnnemiSiBesoinDyn :: Moteur -> Moteur
genererEnnemiSiBesoinDyn m
  | length (mEnnemis m) >= maxEnnemisDynamiques =
      m

  | not (doitGenerer (mTour m) (periodeEnnemi (mTour m))) =
      m

  | otherwise =
      let (x, m1)       = tirerInt (120, largeurZoneJeu - 150) m
          (pv, m2)      = tirerInt (1, 3) m1
          (cadTir, m3)  =
            tirerInt (cadenceTirEnnemiMin, cadenceTirEnnemiMax) m2
          ennemiM       = creerEnnemiDynamique x pv cadTir
      in case ennemiM of
           Nothing ->
             m3

           Just ennemi ->
             m3 { mEnnemis = mEnnemis m3 ++ [ennemi] }

creerEnnemiDynamique :: Int -> Int -> Int -> Maybe Ennemi
creerEnnemiDynamique x pvVal cadTirVal = do
  h <- eitherToMaybe $
    mkRectangle x (hauteurZoneJeu - 80) 30 34

  pv <- eitherToMaybe $
    mkPV pvVal

  cadTir <- eitherToMaybe $
    mkCadence cadTirVal

  eitherToMaybe $
    mkEnnemi h pv (Scripted [Attendre] 0) cadTir

---------------------------------------------------------------------------------
-- Météores
---------------------------------------------------------------------------------

genererMeteoreSiBesoin :: Moteur -> Moteur
genererMeteoreSiBesoin m
  | length (mMeteores m) >= maxMeteoresDynamiques =
      m

  | not (doitGenerer (mTour m) (periodeMeteore (mTour m))) =
      m

  | otherwise =
      let (x, m1)       = tirerInt (100, largeurZoneJeu - 100) m
          (r, m2)       = tirerInt (12, 24) m1
          (cadVal, m3)  = tirerInt (1, 3) m2
          meteoreM      = creerMeteoreDynamique x r cadVal
      in case meteoreM of
           Nothing ->
             m3

           Just meteore ->
             m3 { mMeteores = mMeteores m3 ++ [meteore] }

creerMeteoreDynamique :: Int -> Int -> Int -> Maybe Meteore
creerMeteoreDynamique x rayon cadVal = do
  cad <- eitherToMaybe $
    mkCadence cadVal

  eitherToMaybe $
    mkMeteoreRond x (hauteurZoneJeu + 80) rayon cad

---------------------------------------------------------------------------------
-- Obstacles
---------------------------------------------------------------------------------

genererObstacleSiBesoin :: Moteur -> Moteur
genererObstacleSiBesoin m
  | length (mObstacles m) >= maxObstaclesDynamiques =
      m

  | not (doitGenerer (mTour m) (periodeObstacle (mTour m))) =
      m

  | otherwise =
      let (x, m1)  = tirerInt (100, largeurZoneJeu - 180) m
          (w, m2)  = tirerInt (35, 90) m1
          (h, m3)  = tirerInt (18, 42) m2
          obsM     = creerObstacleDynamique x w h
      in case obsM of
           Nothing ->
             m3

           Just obstacle ->
             m3 { mObstacles = mObstacles m3 ++ [obstacle] }

creerObstacleDynamique :: Int -> Int -> Int -> Maybe Obstacle
creerObstacleDynamique x w h = do
  hitbox <- eitherToMaybe $
    mkRectangle x (hauteurZoneJeu + 60) w h

  eitherToMaybe $
    mkObstacle hitbox

---------------------------------------------------------------------------------
-- Utilitaire
---------------------------------------------------------------------------------

eitherToMaybe :: Either e a -> Maybe a
eitherToMaybe (Right x) =
  Just x
eitherToMaybe (Left _) =
  Nothing

---------------------------------------------------------------------------------
-- Propriétés
---------------------------------------------------------------------------------

prop_pre_genererObjetsDynamiques :: Moteur -> Bool
prop_pre_genererObjetsDynamiques =
  prop_inv_moteur

prop_post_genererObjetsDynamiques :: Moteur -> Moteur -> Bool
prop_post_genererObjetsDynamiques _ m' =
     prop_inv_moteur m'
  && length (mEnnemis m')  <= maxEnnemisDynamiques
  && length (mMeteores m') <= maxMeteoresDynamiques
  && length (mObstacles m') <= maxObstaclesDynamiques