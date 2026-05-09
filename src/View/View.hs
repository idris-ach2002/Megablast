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
import Model.Score (Score, scoreValeur)
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
--   Cette vue dessine uniquement le moteur. Les écrans applicatifs comme
--   l'accueil et le game over sont routés par View.AppView.
dessinerMoteurAvecAssets :: AssetsView -> Moteur -> Picture
dessinerMoteurAvecAssets assets m =
  pictures $
    [ dessinerFondEspace (mTour m)
    , dessinerMursNiveauAvecAssets assets (mTour m) (mMurs m)
    ]
    ++ map dessinerMeteore               (mMeteores m)
    ++ map ObstacleView.dessinerObstacle (mObstacles m)
    ++ map dessinerProjectile            (mProjectiles m)
    ++ map dessinerEnnemi                (mEnnemis m)
    ++ map dessinerVaisseauJoueuse       (mJoueuses m)
    ++ zipWith (dessinerHUDJoueuse (mTour m)) [0 ..] (mJoueuses m)
    ++ dessinerScoresJoueuses            (mScores m)
    ++ [ dessinerTour (mTour m)
       ]
