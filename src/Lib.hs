module Lib
    ( someFunc
    ) where

-- Le modèle de Megablast

someFunc :: IO ()
someFunc = putStrLn "someFunc"

-- modèle d'un pixel, fenêtre hitbox
-- hitbox : c’est un ensemble de pixels qui repréesentent la surface totale couverte par l’objet.
data Pixel = Pixel {pixel_x :: Int , pixel_y :: Int} deriving (Show, Eq, Ord)
data Pane = Pane {largeur:: Int, hauteur :: Int} deriving (Show, Eq)

-- Taille de la fenêtre par défaut 800 x 500
main_pane :: Pane
main_pane = Pane 800 500

data Hitbox = Point Int Int
    | Disque Int Int Int
    | Rectangle Int Int Int Int
    | Composee [Hitbox] 
    deriving (Eq, Show)

prop_inv_hitbox :: Hitbox -> Bool
prop_inv_hitbox (Point x y) = x >= 0 && x <= (largeur main_pane) && y >= 0 && y <= (hauteur main_pane)
prop_inv_hitbox (Disque xc yc r) = r > 0 && (xc - r) >= 0 && (xc + r) <= (largeur main_pane) && (yc - r) >= 0 && (yc + r) <= (hauteur main_pane)