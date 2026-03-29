{- HLINT ignore "Use camelCase" -}
{-# LANGUAGE OverloadedStrings #-}

module Hitbox where

import Data.Text (Text)
import Lib


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
--TODO: Revoir les invariants sur les murs, 
prop_inv_hitbox (MurGauche l) = length l >= 2
prop_inv_hitbox (MurDroit l) = length l >= 2

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

-- Collisions : cas demandés dans l'ER1
--TODO: compélter tous les cas de collisions

-- appropriate_segment (Point x y) [(Int, Int)] : Trouve les deux points (le segment) entre lesquels notre Point x y se trouve.
point_appropriate_segment :: Hitbox -> [(Int, Int)] -> ((Int,Int), (Int, Int))
point_appropriate_segment p@(Point _ y) (p1@(_, y1) : p2@(_, y2) : l') = 
  if y >= y1 && y <= y2 then (p1, p2)
  else point_appropriate_segment  p (p2 : l')
-- TODO: jsp si vaut mieux rendre faux pour les cas qui ne sont pas censé arrivé, en tt cas on met undefined pour voir au moins quand ça crash
point_appropriate_segment _ _ = undefined -- Ce cas n'est jamais censé arrivé 

collision :: Hitbox -> Hitbox -> Bool
collision (Point x1 y1) (Point x2 y2) =
  x1 == x2 && y1 == y2

collision (Rectangle x1 y1 w1 h1) (Rectangle x2 y2 w2 h2) =
  x1 < x2 + w2 && x2 < x1 + w1 && y1 < y2 + h2 && y2 < y1 + h1

collision (Point x y) (Disque xc yc r) =
  let dx = fromIntegral (x - xc) :: Float
      dy = fromIntegral (y - yc) :: Float
  in sqrt (dx*dx + dy*dy) <= fromIntegral r
collision d@(Disque _ _ _) p@(Point _ _) = collision p d

collision (Composee hs) h = any (`collision` h) hs
collision h (Composee hs) = any (collision h) hs

collision p@(Point x y) (MurGauche l) = collision' $ point_appropriate_segment p l
  where 
    collision' ((x1, y1), (x2, y2)) = 

      let
        -- Calcule de l'équation de la droite du segment
        -- y = ax + b 
        aSeg :: Float
        aSeg = fromIntegral (y2 - y1) / fromIntegral (x2 - x1)        
        -- On replace avec un des deux points du segment pour avoir b d'après: b = y - ax
        bSeg :: Float
        bSeg = fromIntegral y1 - aSeg * fromIntegral x1
        -- Calcule du point (x', y') qui qui représente la projection horizontale du point (x,y) sur le segment.
        -- On a y' = y, Pour x' : On remplace dans la droite du segment: x' = (y'-b) / a
        x' :: Float
        x' = (fromIntegral y - bSeg) / aSeg
      in
      -- On est en collision si on se trouve à gauche de la projection horizontale
      fromIntegral x <= x'
--TODO: factorise avec celui de gauche
collision p@(Point x y) (MurDroit l) = collision' $ point_appropriate_segment p l
  where 
    collision' ((x1, y1), (x2, y2)) = 

      let
        -- Calcule de l'équation de la droite du segment
        -- y = ax + b 
        aSeg :: Float
        aSeg = fromIntegral (y2 - y1) / fromIntegral (x2 - x1)        
        -- On replace avec un des deux points du segment pour avoir b d'après: b = y - ax
        bSeg :: Float
        bSeg = fromIntegral y1 - aSeg * fromIntegral x1
        -- Calcule du point (x', y') qui qui représente la projection horizontale du point (x,y) sur le segment.
        -- On a y' = y, Pour x' : On remplace dans la droite du segment: x' = (y'-b) / a
        x' :: Float
        x' = (fromIntegral y - bSeg) / aSeg
      in
      -- On est en collision si on se trouve à droite de la projection horizontale
      fromIntegral x >= x'

collision _ _ = undefined

-- Q1.4 : si h1 = Composee [p1,p2] et h2 est un Point en collision avec h1,
-- alors h2 est p1 ou p2.
prop_composee2points_point :: (Int, Int) -> (Int, Int) -> (Int, Int) -> Bool
prop_composee2points_point (x1,y1) (x2,y2) (x,y) =
  let p1 = Point x1 y1
      p2 = Point x2 y2
      h1 = Composee [p1,p2]
      h2 = Point x y
  in collision h1 h2 ==> (h2 == p1 || h2 == p2)
  where
    (==>) a b = (not a) || b


