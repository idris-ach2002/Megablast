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
import View.HUDView
import qualified View.ObstacleView as ObstacleView

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


dessinerProjectile :: Projectile -> Picture
dessinerProjectile p =
  let c =
        case prOwner p of
          TirJoueuse -> Theme.couleurProjJoueuse
          TirEnnemi  -> Theme.couleurProjEnnemi
  in HB.dessinerHitbox c (prHitbox p)

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
        ++ map ObstacleView.dessinerObstacle (mObstacles m)        ++ map dessinerProjectile      (mProjectiles m)
        ++ map dessinerEnnemi          (mEnnemis m)
        ++ map dessinerVaisseauJoueuse (mJoueuses m)
        ++ zipWith (dessinerHUDJoueuse (mTour m)) [0 ..] (mJoueuses m)
        ++ [ dessinerScore (mScore m)
          , dessinerTour (mTour m)
          ]