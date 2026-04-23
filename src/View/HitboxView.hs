{-# LANGUAGE NamedFieldPuns #-}

module View.HitboxView where

import Graphics.Gloss
import Model.Hitbox

---------------------------------------------------------------------------------
-- Conversion de coordonnées
---------------------------------------------------------------------------------

largeurFenetre, hauteurFenetre :: Int
largeurFenetre = 800
hauteurFenetre = 900

rayonPoint :: Float
rayonPoint = 3

toGloss :: Int -> Int -> (Float, Float)
toGloss x y =
  ( fromIntegral x - fromIntegral largeurFenetre  / 2
  , fromIntegral y - fromIntegral hauteurFenetre / 2
  )

---------------------------------------------------------------------------------
-- Primitives de dessin liées aux hitbox
---------------------------------------------------------------------------------

dessinerPointSolid :: Color -> Int -> Int -> Picture
dessinerPointSolid c x y =
  let (gx, gy) = toGloss x y
  in color c $ translate gx gy $ circleSolid rayonPoint

dessinerDisqueSolid :: Color -> Int -> Int -> Int -> Picture
dessinerDisqueSolid c xc yc r =
  let (gx, gy) = toGloss xc yc
  in color c $ translate gx gy $ circleSolid (fromIntegral r)

dessinerRectangleSolid :: Color -> Int -> Int -> Int -> Int -> Picture
dessinerRectangleSolid c x y w h =
  let (gx, gy) = toGloss x y
      fw = fromIntegral w
      fh = fromIntegral h
  in color c $ translate (gx + fw / 2) (gy + fh / 2) $ rectangleSolid fw fh

dessinerRectangleWire :: Color -> Int -> Int -> Int -> Int -> Picture
dessinerRectangleWire c x y w h =
  let (gx, gy) = toGloss x y
      fw = fromIntegral w
      fh = fromIntegral h
  in color c $ translate (gx + fw / 2) (gy + fh / 2) $ rectangleWire fw fh

dessinerPolygoneJeu :: Color -> [(Int, Int)] -> Picture
dessinerPolygoneJeu c pts =
  color c $ polygon [ toGloss x y | (x, y) <- pts ]

---------------------------------------------------------------------------------
-- Rendu générique des hitbox
---------------------------------------------------------------------------------

dessinerHitbox :: Color -> Hitbox -> Picture
dessinerHitbox c (Point x y) =
  dessinerPointSolid c x y

dessinerHitbox c (Disque xc yc r) =
  dessinerDisqueSolid c xc yc r

dessinerHitbox c (Model.Hitbox.Rectangle x y w h) =
  dessinerRectangleSolid c x y w h

dessinerHitbox c (Composee hs) =
  pictures $ map (dessinerHitbox c) hs

dessinerHitbox c (MurGauche pts) =
  pictures $ zipWith dessinerSegmentGauche pts (tail pts)
  where
    dessinerSegmentGauche (x1, y1) (x2, y2) =
      let (_, gy1) = toGloss x1 y1
          (gx1, _) = toGloss x1 y1
          (_, gy2) = toGloss x2 y2
          (gx2, _) = toGloss x2 y2
          bord     = fromIntegral (- largeurFenetre `div` 2)
      in color c $ polygon [(bord, gy1), (gx1, gy1), (gx2, gy2), (bord, gy2)]

dessinerHitbox c (MurDroit pts) =
  pictures $ zipWith dessinerSegmentDroit pts (tail pts)
  where
    dessinerSegmentDroit (x1, y1) (x2, y2) =
      let (_, gy1) = toGloss x1 y1
          (gx1, _) = toGloss x1 y1
          (_, gy2) = toGloss x2 y2
          (gx2, _) = toGloss x2 y2
          bord     = fromIntegral (largeurFenetre `div` 2)
      in color c $ polygon [(gx1, gy1), (bord, gy1), (bord, gy2), (gx2, gy2)]
