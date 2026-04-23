module View.FormesView where

import Graphics.Gloss
import qualified View.HitboxView as HB

---------------------------------------------------------------------------------
-- Formes visuelles qui ne correspondent pas directement à des hitbox
---------------------------------------------------------------------------------

-- | Ligne fermée à partir de points du jeu.
dessinerLigneFermeeJeu :: Color -> [(Int, Int)] -> Picture
dessinerLigneFermeeJeu c pts =
  color c $ lineLoop [ HB.toGloss x y | (x, y) <- pts ]

-- | Triangle quelconque à partir de ses trois sommets en coordonnées du jeu.
dessinerTriangleJeu :: Color -> (Int, Int) -> (Int, Int) -> (Int, Int) -> Picture
dessinerTriangleJeu c a b d =
  HB.dessinerPolygoneJeu c [a, b, d]

-- | Triangle isocèle orienté vers le haut, contenu dans la boîte (x,y,w,h).
triangleIsoceleHautPoints :: Int -> Int -> Int -> Int -> [(Int, Int)]
triangleIsoceleHautPoints x y w h =
  [ (x, y)
  , (x + w, y)
  , (x + w `div` 2, y + h)
  ]

dessinerTriangleIsoceleHaut :: Color -> Int -> Int -> Int -> Int -> Picture
dessinerTriangleIsoceleHaut c x y w h =
  HB.dessinerPolygoneJeu c (triangleIsoceleHautPoints x y w h)

-- | Triangle isocèle orienté vers le bas, contenu dans la boîte (x,y,w,h).
triangleIsoceleBasPoints :: Int -> Int -> Int -> Int -> [(Int, Int)]
triangleIsoceleBasPoints x y w h =
  [ (x, y + h)
  , (x + w, y + h)
  , (x + w `div` 2, y)
  ]

dessinerTriangleIsoceleBas :: Color -> Int -> Int -> Int -> Int -> Picture
dessinerTriangleIsoceleBas c x y w h =
  HB.dessinerPolygoneJeu c (triangleIsoceleBasPoints x y w h)

-- | Triangle rectangle dont l'angle droit est en bas à gauche.
triangleRectangleBasGauchePoints :: Int -> Int -> Int -> Int -> [(Int, Int)]
triangleRectangleBasGauchePoints x y w h =
  [ (x, y)
  , (x + w, y)
  , (x, y + h)
  ]

dessinerTriangleRectangleBasGauche :: Color -> Int -> Int -> Int -> Int -> Picture
dessinerTriangleRectangleBasGauche c x y w h =
  HB.dessinerPolygoneJeu c (triangleRectangleBasGauchePoints x y w h)

-- | Triangle rectangle dont l'angle droit est en bas à droite.
triangleRectangleBasDroitPoints :: Int -> Int -> Int -> Int -> [(Int, Int)]
triangleRectangleBasDroitPoints x y w h =
  [ (x, y)
  , (x + w, y)
  , (x + w, y + h)
  ]

dessinerTriangleRectangleBasDroit :: Color -> Int -> Int -> Int -> Int -> Picture
dessinerTriangleRectangleBasDroit c x y w h =
  HB.dessinerPolygoneJeu c (triangleRectangleBasDroitPoints x y w h)

-- | Triangle rectangle dont l'angle droit est en haut à gauche.
triangleRectangleHautGauchePoints :: Int -> Int -> Int -> Int -> [(Int, Int)]
triangleRectangleHautGauchePoints x y w h =
  [ (x, y)
  , (x, y + h)
  , (x + w, y + h)
  ]

dessinerTriangleRectangleHautGauche :: Color -> Int -> Int -> Int -> Int -> Picture
dessinerTriangleRectangleHautGauche c x y w h =
  HB.dessinerPolygoneJeu c (triangleRectangleHautGauchePoints x y w h)

-- | Triangle rectangle dont l'angle droit est en haut à droite.
triangleRectangleHautDroitPoints :: Int -> Int -> Int -> Int -> [(Int, Int)]
triangleRectangleHautDroitPoints x y w h =
  [ (x + w, y)
  , (x, y + h)
  , (x + w, y + h)
  ]

dessinerTriangleRectangleHautDroit :: Color -> Int -> Int -> Int -> Int -> Picture
dessinerTriangleRectangleHautDroit c x y w h =
  HB.dessinerPolygoneJeu c (triangleRectangleHautDroitPoints x y w h)

-- | Triangle équilatéral approximé à partir du sommet bas-gauche et du côté.
triangleEquilateralHautPoints :: Int -> Int -> Int -> [(Int, Int)]
triangleEquilateralHautPoints x y cote =
  let h = round (fromIntegral cote * sqrt 3 / 2 :: Float)
  in [ (x, y)
     , (x + cote, y)
     , (x + cote `div` 2, y + h)
     ]

dessinerTriangleEquilateralHaut :: Color -> Int -> Int -> Int -> Picture
dessinerTriangleEquilateralHaut c x y cote =
  HB.dessinerPolygoneJeu c (triangleEquilateralHautPoints x y cote)
