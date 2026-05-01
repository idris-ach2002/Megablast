module View.MeteoreView (dessinerMeteore) where

import Graphics.Gloss
import Model.Hitbox
import Model.Meteore
import qualified View.HitboxView as HB
import qualified View.Theme as Theme

dessinerMeteore :: Meteore -> Picture
dessinerMeteore mt =
  case mtHitbox mt of
    -- On convertit x, y, r en Float une seule fois ici
    Disque x y r -> 
      let (gx, gy) = HB.toGloss x y
          fr = fromIntegral r
      in translate gx gy $ dessinerMeteoreRelatif fr
    h -> HB.dessinerHitbox Theme.couleurMeteore h

-- Cette fonction dessine un météore centré en (0,0)
dessinerMeteoreRelatif :: Float -> Picture
dessinerMeteoreRelatif r =
  pictures
    [ dessinerPoussiere r
    , color Theme.couleurMeteoreBord $ polygon (pointsRoche (r + r/8))
    , color Theme.couleurMeteore     $ polygon (pointsRoche r)
    , dessinerDetails r
    ]

-- Les points sont définis par rapport à r (le centre est 0,0)
pointsRoche :: Float -> [(Float, Float)]
pointsRoche r = 
  [ (-r,      r/5) , (-r/2,    r)   , (r/4,     r)
  , (r,       r/3) , (r/2,    -r)   , (-r/4,   -r)
  , (-r,     -r/2)
  ]

dessinerDetails :: Float -> Picture
dessinerDetails r = pictures
    -- Zones de relief
    [ detail Theme.couleurMeteoreClair (-r/3) (r/3)  (r/4)
    , detail Theme.couleurMeteoreOmbre (r/3)  (-r/4) (r/3)
    -- Cratères
    , detail Theme.couleurMeteoreTrou  (r/4)  (r/6)  (r/5)
    , detail Theme.couleurMeteoreTrou  (-r/3) (-r/3) (r/6)
    -- Fissures (utilisent line au lieu de polygon)
    , color Theme.couleurMeteoreBord $ line [(-r/2, 0), (0, -r/5), (r/3, -r/2)]
    ]
  where
    detail c dx dy dr = translate dx dy $ color c $ circleSolid dr

dessinerPoussiere :: Float -> Picture
dessinerPoussiere r = color Theme.couleurMeteorePoussiere $ pictures
    [ translate (-r/2) (r + 14) $ circleSolid (r/8)
    , translate (r/2)  (r + 28) $ circleSolid (r/9)
    , translate 0      (r + 42) $ circleSolid 1
    ]