module View.ObstacleView
  ( dessinerObstacle
  ) where

import Graphics.Gloss
import Model.Hitbox
import Model.Objects
import qualified View.HitboxView as HB
import qualified View.Theme as Theme

---------------------------------------------------------------------------------
-- Rendu des obstacles
---------------------------------------------------------------------------------

dessinerObstacle :: Obstacle -> Picture
dessinerObstacle (Obstacle hbx) =
  case hbx of
    Model.Hitbox.Rectangle x y w h ->
      -- 1. On traduit une seule fois et on passe en Float
      let (gx, gy) = HB.toGloss x y
      in translate gx gy $ dessinerObstacleMetal (fromIntegral w) (fromIntegral h)
    _ -> HB.dessinerHitbox Theme.couleurObstacle hbx

dessinerObstacleMetal :: Float -> Float -> Picture
dessinerObstacleMetal w h =
  pictures
    [ rectSolid Theme.couleurObstacleOmbre  0 0 w h
    , rectSolid Theme.couleurObstacleCorps  2 2 (w - 4) (h - 4)
    , rectSolid Theme.couleurObstaclePlaque 6 6 (w - 12) (h - 12)
    , dessinerBandes w h
    , dessinerRivets w h
    , rectWire  Theme.couleurObstacleBord   0 0 w h
    ]

dessinerBandes :: Float -> Float -> Picture
dessinerBandes w h =
  pictures
    [ rectSolid Theme.couleurObstacleLigne 0 (h / 3)     w 2
    , rectSolid Theme.couleurObstacleLigne 0 (2 * h / 3) w 2
    ]

dessinerRivets :: Float -> Float -> Picture
dessinerRivets w h =
  pictures
    -- Plus besoin de x et y, on se repère uniquement par rapport à la largeur/hauteur !
    [ rivet 8       8
    , rivet (w - 8) 8
    , rivet 8       (h - 8)
    , rivet (w - 8) (h - 8)
    ]

rivet :: Float -> Float -> Picture
rivet rx ry =
  translate rx ry $ pictures
    [ color Theme.couleurObstacleRivetBord $ circleSolid 4
    , color Theme.couleurObstacleRivet     $ circleSolid 2
    ]

---------------------------------------------------------------------------------
-- Primitives locales (Origine = Coin)
---------------------------------------------------------------------------------

-- Gloss dessine les rectangles depuis leur centre.
-- Ces fonctions nous permettent de les dessiner depuis un coin (rx, ry)
-- pour garder une logique de construction simple.

rectSolid :: Color -> Float -> Float -> Float -> Float -> Picture
rectSolid c rx ry rw rh =
  let rw' = max 1 rw
      rh' = max 1 rh
  in color c $ translate (rx + rw'/2) (ry + rh'/2) $ rectangleSolid rw' rh'

rectWire :: Color -> Float -> Float -> Float -> Float -> Picture
rectWire c rx ry rw rh =
  let rw' = max 1 rw
      rh' = max 1 rh
  in color c $ translate (rx + rw'/2) (ry + rh'/2) $ rectangleWire rw' rh'