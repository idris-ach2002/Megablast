module View.MeteoreView
  ( dessinerMeteore
  ) where

import Graphics.Gloss
import Model.Hitbox
import Model.Meteore
import qualified View.HitboxView as HB
import qualified View.Theme as Theme
import View.FormesView

---------------------------------------------------------------------------------
-- Rendu des météores
---------------------------------------------------------------------------------

dessinerMeteore :: Meteore -> Picture
dessinerMeteore mt =
  case mtHitbox mt of
    Disque x y r -> dessinerMeteoreRond x y r
    h            -> HB.dessinerHitbox Theme.couleurMeteore h

dessinerMeteoreRond :: Int -> Int -> Int -> Picture
dessinerMeteoreRond x y r =
  let (gx, gy) = HB.toGloss x y
      rf = fromIntegral r
  in pictures
      [ dessinerTrainee x y r
      , color Theme.couleurMeteoreBord  $ translate gx gy $ circleSolid (rf * 1.05)
      , color Theme.couleurMeteore      $ translate gx gy $ circleSolid rf
      , color Theme.couleurMeteoreClair $ translate (gx - rf * 0.25) (gy + rf * 0.20) $ circleSolid (rf * 0.35)
      , color Theme.couleurMeteoreTrou  $ translate (gx + rf * 0.30) (gy - rf * 0.20) $ circleSolid (max 1.5 (rf * 0.18))
      , color Theme.couleurMeteoreTrou  $ translate (gx - rf * 0.35) (gy - rf * 0.30) $ circleSolid (max 1.0 (rf * 0.12))
      ]

dessinerTrainee :: Int -> Int -> Int -> Picture
dessinerTrainee x y r =
  dessinerTriangleJeu
    Theme.couleurMeteoreTrainee
    (x - largeurBase, y + hauteurBase)
    (x,               y + longueur)
    (x + largeurBase, y + hauteurBase)
  where
    largeurBase = max 4 (r `div` 2)
    hauteurBase = max 2 (r `div` 3)
    longueur    = max 30 (r * 4)