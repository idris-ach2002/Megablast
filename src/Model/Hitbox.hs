{- HLINT ignore "Use camelCase" -}
{-# LANGUAGE OverloadedStrings #-}

module Model.Hitbox where

import Data.Text (Text)
import Model.Lib

data Hitbox
  = Point Int Int
  | Disque Int Int Int
  | Rectangle Int Int Int Int
  | MurGauche [(Int, Int)]
  | MurDroit [(Int, Int)]
  | Composee [Hitbox]
  deriving (Eq, Show)

-- Invariant structurel
prop_inv_hitbox :: Hitbox -> Bool
prop_inv_hitbox (Point _ _)         = True
prop_inv_hitbox (Disque _ _ r)      = r > 0
prop_inv_hitbox (Rectangle _ _ w h) = w > 0 && h > 0
prop_inv_hitbox (Composee hs)       = length hs >= 2 && all prop_inv_hitbox hs
--TODO: idéalement l'invariant c'est que les coordonnées soit tous > (comment approximer le mieux ?)
prop_inv_hitbox (MurGauche ((_, y1) : (_, y2) : _)) = y1 < y2
prop_inv_hitbox (MurGauche _)                       = False
prop_inv_hitbox (MurDroit  ((_, y1) : (_, y2) : _)) = y1 < y2
prop_inv_hitbox (MurDroit  _)                       = False

-- Smart constructors
mkPoint :: Int -> Int -> Hitbox
mkPoint = Point

mkDisque :: Int -> Int -> Int -> Either Text Hitbox
mkDisque xc yc r
  | r > 0     = Right (Disque xc yc r)
  | otherwise = Left ("Rayon non strictement positif: r=" <> tshow r)

mkRectangle :: Int -> Int -> Int -> Int -> Either Text Hitbox
mkRectangle x y w h
  | w > 0 && h > 0 = Right (Rectangle x y w h)
  | otherwise      = Left ("Rectangle invalide: w=" <> tshow w <> ", h=" <> tshow h)

mkComposee :: [Hitbox] -> Either Text Hitbox
mkComposee hs
  | length hs < 2          = Left "Une hitbox Composee doit avoir au moins 2 composantes"
  | all prop_inv_hitbox hs = Right (Composee hs)
  | otherwise              = Left "Une composante viole prop_inv_hitbox"

pointInRectangle :: Int -> Int -> Int -> Int -> Int -> Int -> Bool
pointInRectangle x y rx ry w h =
  rx <= x && x < rx + w && ry <= y && y < ry + h

clampInt :: Int -> Int -> Int -> Int
clampInt lo hi v = max lo (min hi v)

distanceSquared :: Int -> Int -> Int -> Int -> Int
distanceSquared x1 y1 x2 y2 =
  let dx = x1 - x2
      dy = y1 - y2
  in dx * dx + dy * dy
  
collisionRectangleDisque :: Int -> Int -> Int -> Int -> Int -> Int -> Int -> Bool
collisionRectangleDisque rx ry w h xc yc r =
  let closestX = clampInt rx (rx + w) xc
      closestY = clampInt ry (ry + h) yc
  in distanceSquared xc yc closestX closestY <= r * r

segmentOverlapsY :: Int -> Int -> ((Int, Int), (Int, Int)) -> Bool
segmentOverlapsY yMin yMax ((_, y1), (_, y2)) =
  max yMin y1 <= min yMax y2

-- Trouve le segment du mur qui encadre l'ordonnée donnée.
-- Renvoie Nothing si l'ordonnée n'est couverte par aucun segment.
segmentAtY :: Int -> [(Int, Int)] -> Maybe ((Int, Int), (Int, Int))
segmentAtY _ [] = Nothing
segmentAtY _ [_] = Nothing
segmentAtY y (p1@(_, y1) : p2@(_, y2) : l')
  | y >= y1 && y <= y2 = Just (p1, p2)
  | otherwise          = segmentAtY y (p2 : l')

-- Abscisse du point du segment d'ordonnée y.
-- Cette écriture par interpolation sur y gère naturellement les segments verticaux.
xOnSegmentAtY :: Int -> ((Int, Int), (Int, Int)) -> Float
xOnSegmentAtY y ((x1, y1), (x2, y2))
  | y1 == y2  = fromIntegral x1
  | otherwise =
      let t :: Float
          t = fromIntegral (y - y1) / fromIntegral (y2 - y1)
      in fromIntegral x1 + t * fromIntegral (x2 - x1)

collisionPointMur :: (Float -> Float -> Bool) -> Int -> Int -> [(Int, Int)] -> Bool
collisionPointMur cmp x y pts =
  case segmentAtY y pts of
    Nothing  -> False
    Just seg -> cmp (fromIntegral x) (xOnSegmentAtY y seg)

collisionRectangleMur
  :: (Float -> Float -> Bool)
  -> (Float -> Float -> Float)
  -> Int
  -> Int
  -> Int
  -> [(Int, Int)]
  -> Bool
collisionRectangleMur cmp chooseX edgeX ry h pts =
  let yMin = ry
      yMax = ry + h - 1

      aux [] = False
      aux [_] = False
      aux (p1@(_, y1) : p2@(_, y2) : rest)
        | y2 < yMin = aux (p2 : rest)
        | y1 > yMax = False
        | otherwise =
            let seg    = (p1, p2)
                yStart = max yMin y1
                yEnd   = min yMax y2
                xStart = xOnSegmentAtY yStart seg
                xEnd   = xOnSegmentAtY yEnd seg
                wallX  = chooseX xStart xEnd
            in cmp (fromIntegral edgeX) wallX

  in aux pts
  
--TODO: compléter progressivement les autres cas utiles
collision :: Hitbox -> Hitbox -> Bool
collision (Point x1 y1) (Point x2 y2) =
  x1 == x2 && y1 == y2

collision (Point x y) (Rectangle rx ry w h) =
  pointInRectangle x y rx ry w h
collision r@(Rectangle _ _ _ _) p@(Point _ _) = collision p r

collision (Rectangle x1 y1 w1 h1) (Rectangle x2 y2 w2 h2) =
  x1 < x2 + w2 && x2 < x1 + w1 && y1 < y2 + h2 && y2 < y1 + h1

collision (Point x y) (Disque xc yc r) =
  let dx = fromIntegral (x - xc) :: Float
      dy = fromIntegral (y - yc) :: Float
  in sqrt (dx * dx + dy * dy) <= fromIntegral r
collision d@(Disque _ _ _) p@(Point _ _) = collision p d

collision (Disque x1 y1 r1) (Disque x2 y2 r2) =
  distanceSquared x1 y1 x2 y2 <= (r1 + r2) * (r1 + r2)

collision (Rectangle rx ry w h) (Disque xc yc r) =
  collisionRectangleDisque rx ry w h xc yc r
collision d@(Disque _ _ _) r@(Rectangle _ _ _ _) = collision r d

collision (Composee hs) h = any (`collision` h) hs
collision h (Composee hs) = any (collision h) hs

collision (Point x y) (MurGauche l) = collisionPointMur (<=) x y l
collision mur@(MurGauche _) p@(Point _ _) = collision p mur

collision (Point x y) (MurDroit l) = collisionPointMur (>=) x y l
collision mur@(MurDroit _) p@(Point _ _) = collision p mur

collision (Rectangle rx ry _ h) (MurGauche l) =
  collisionRectangleMur (<=) max rx ry h l
collision mur@(MurGauche _) rect@(Rectangle _ _ _ _) = collision rect mur

collision (Rectangle rx ry w h) (MurDroit l) =
  collisionRectangleMur (>=) min (rx + w - 1) ry h l
collision mur@(MurDroit _) rect@(Rectangle _ _ _ _) = collision rect mur
--TODO
collision _ _ = False

-- Q1.4 : si h1 = Composee [p1,p2] et h2 est un Point en collision avec h1,
-- alors h2 est un des deux points de h1.
prop_composee2points_point :: (Int, Int) -> (Int, Int) -> (Int, Int) -> Bool
prop_composee2points_point (x1,y1) (x2,y2) (x,y) =
  let p1 = Point x1 y1
      p2 = Point x2 y2
      h1 = Composee [p1,p2]
      h2 = Point x y
  in collision h1 h2 ==> (h2 == p1 || h2 == p2)
  where
    (==>) a b = (not a) || b
