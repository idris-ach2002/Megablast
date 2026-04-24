module View.View
  ( largeurFenetre
  , hauteurFenetre
  , couleurFond
  , dessinerMoteur
  ) where

import Graphics.Gloss
import Model.Engine
import Model.Objects
import qualified View.HitboxView as HB
import View.MurView
import qualified View.Theme as Theme
import View.VaisseauView

---------------------------------------------------------------------------------
-- Façade pour Main
---------------------------------------------------------------------------------

largeurFenetre, hauteurFenetre :: Int
largeurFenetre = HB.largeurFenetre
hauteurFenetre = HB.hauteurFenetre

couleurFond :: Color
couleurFond = Theme.couleurFond

---------------------------------------------------------------------------------
-- Rendu des autres entités
---------------------------------------------------------------------------------

dessinerObstacle :: Obstacle -> Picture
dessinerObstacle (Obstacle h) = HB.dessinerHitbox Theme.couleurObstacle h

dessinerProjectile :: Projectile -> Picture
dessinerProjectile p =
  let c = case prOwner p of
            TirJoueuse -> Theme.couleurProjJoueuse
            TirEnnemi  -> Theme.couleurProjEnnemi
  in HB.dessinerHitbox c (prHitbox p)

dessinerEnnemi :: Ennemi -> Picture
dessinerEnnemi e = HB.dessinerHitbox Theme.couleurEnnemi (eHitbox e)

---------------------------------------------------------------------------------
-- HUD
---------------------------------------------------------------------------------

dessinerHUD :: Int -> VaisseauJoueuse -> Picture
dessinerHUD idx v =
  let xBase  = fromIntegral (- largeurFenetre `div` 2) + 10
      yBase  = fromIntegral (hauteurFenetre `div` 2) - 20 - fromIntegral (idx * 25)
      pvTxt  = "PV: " ++ show (vjPv v)
      essTxt = "  Essais: " ++ show (vjEssais v)
      txt    = "J" ++ show (idx + 1) ++ "  " ++ pvTxt ++ essTxt
  in color white $ translate xBase yBase $ scale 0.13 0.13 $ text txt

dessinerTour :: Int -> Picture
dessinerTour t =
  let x = fromIntegral (largeurFenetre `div` 2) - 120
      y = fromIntegral (- hauteurFenetre `div` 2) + 8
  in color (greyN 0.6) $ translate x y $ scale 0.10 0.10 $ text ("Tour: " ++ show t)

dessinerGameOver :: Picture
dessinerGameOver =
  pictures
    [ color (makeColorI 0 0 0 180) $ rectangleSolid (fromIntegral largeurFenetre)
                                                     (fromIntegral hauteurFenetre)
    , color red   $ translate (-140) 20    $ scale 0.4 0.4 $ text "GAME OVER"
    , color white $ translate (-110) (-30) $ scale 0.15 0.15 $ text "Appuyez sur R pour recommencer"
    ]

---------------------------------------------------------------------------------
-- Rendu principal du moteur
---------------------------------------------------------------------------------

dessinerMoteur :: Moteur -> Picture
dessinerMoteur m
  | not (prop_partie_en_cours m) = dessinerGameOver
  | otherwise =
      pictures $
        [ color Theme.couleurFond $ rectangleSolid fw fh
        , dessinerMursNiveau (mMurs m)
        ]
        ++ map dessinerObstacle        (mObstacles m)
        ++ map dessinerProjectile      (mProjectiles m)
        ++ map dessinerEnnemi          (mEnnemis m)
        ++ map dessinerVaisseauJoueuse (mJoueuses m)
        ++ zipWith dessinerHUD [0 ..]  (mJoueuses m)
        ++ [dessinerTour (mTour m)]
  where
    fw = fromIntegral largeurFenetre
    fh = fromIntegral hauteurFenetre
