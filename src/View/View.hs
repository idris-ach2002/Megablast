module View.View
  ( largeurFenetre
  , hauteurFenetre
  , couleurFond
  , dessinerMoteur
  , dessinerMoteurAvecAssets
  ) where

import Graphics.Gloss
import Model.Engine
import Model.Objects
import qualified View.HitboxView as HB
import View.MurView
import qualified View.Theme as Theme
import View.VaisseauView
import View.Background
import View.MeteoreView
import View.Assets
import View.EnnemiView
import Model.Score

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
dessinerObstacle (Obstacle h) =
  HB.dessinerHitbox Theme.couleurObstacle h

dessinerProjectile :: Projectile -> Picture
dessinerProjectile p =
  let c =
        case prOwner p of
          TirJoueuse -> Theme.couleurProjJoueuse
          TirEnnemi  -> Theme.couleurProjEnnemi
  in HB.dessinerHitbox c (prHitbox p)


dessinerHUD :: Int -> VaisseauJoueuse -> Picture
dessinerHUD idx v =
  let xBase  = fromIntegral (- largeurFenetre `div` 2) + 10
      yBase  = fromIntegral (hauteurFenetre `div` 2) - 20 - fromIntegral (idx * 25)
      pvTxt  = "PV: " ++ show (vjPv v)
      essTxt = "  Essais: " ++ show (vjEssais v)
      txt    = "J" ++ show (idx + 1) ++ "  " ++ pvTxt ++ essTxt
  in color white $
       translate xBase yBase $
         scale 0.13 0.13 $
           text txt

dessinerScore :: Score -> Picture
dessinerScore score =
  let x = fromIntegral (- largeurFenetre `div` 2) + 10
      y = fromIntegral (- hauteurFenetre `div` 2) + 8
      txt = "Score: " ++ show (scoreValeur score)
  in color (greyN 0.85) $
       translate x y $
         scale 0.12 0.12 $
           text txt

dessinerTour :: Int -> Picture
dessinerTour t =
  let x = fromIntegral (largeurFenetre `div` 2) - 120
      y = fromIntegral (- hauteurFenetre `div` 2) + 8
  in color (greyN 0.6) $
       translate x y $
         scale 0.10 0.10 $
           text ("Tour: " ++ show t)

dessinerGameOver :: Picture
dessinerGameOver =
  pictures
    [ color (makeColorI 0 0 0 180) $
        rectangleSolid
          (fromIntegral largeurFenetre)
          (fromIntegral hauteurFenetre)

    , color red $
        translate (-140) 20 $
          scale 0.4 0.4 $
            text "GAME OVER"

    , color white $
        translate (-110) (-30) $
          scale 0.15 0.15 $
            text "Appuyez sur R pour recommencer"
    ]

---------------------------------------------------------------------------------
-- Rendu principal du moteur
---------------------------------------------------------------------------------

-- | Rendu historique sans assets chargés.
--   Il reste utile pour garder une API simple et un fallback.
dessinerMoteur :: Moteur -> Picture
dessinerMoteur =
  dessinerMoteurAvecAssets assetsVides

assetsVides :: AssetsView
assetsVides =
  AssetsView
    { assetMurGauche = Nothing
    , assetMurDroit  = Nothing
    }

-- | Rendu principal avec assets graphiques.
dessinerMoteurAvecAssets :: AssetsView -> Moteur -> Picture
dessinerMoteurAvecAssets assets m
  | not (prop_partie_en_cours m) =
      pictures
        [ dessinerFondEspace (mTour m)
        , dessinerScore (mScore m)
        , dessinerTour (mTour m)
        , dessinerGameOver
        ]

  | otherwise =
      pictures $
        [ dessinerFondEspace (mTour m)
        , dessinerMursNiveauAvecAssets assets (mTour m) (mMurs m)
        ]
        ++ map dessinerMeteore         (mMeteores m)
        ++ map dessinerObstacle        (mObstacles m)
        ++ map dessinerProjectile      (mProjectiles m)
        ++ map dessinerEnnemi          (mEnnemis m)
        ++ map dessinerVaisseauJoueuse (mJoueuses m)
        ++ zipWith dessinerHUD [0 ..]  (mJoueuses m)
        ++ [ dessinerScore (mScore m)
           , dessinerTour (mTour m)
           ]