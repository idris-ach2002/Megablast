{-# LANGUAGE OverloadedStrings #-}
{- HLINT ignore "Redundant bracket" -}
{- HLINT ignore "Eta reduce" -}
{- HLINT ignore "Use record patterns" -}
{- HLINT ignore "Use camelCase" -}

module Lib
  ( someFunc
  -- Utilitaires
  , tshow

  -- Partie 1 : Hitbox
  , Hitbox(..)
  , prop_inv_hitbox
  , mkPoint, mkDisque, mkRectangle, mkComposee
  , collision
  , prop_composee2points_point
  -- Partie 2: murs
  ,mur_dent_scie_Exam
  ,mur_gauche_vertical_10

  -- Partie 3 : Obstacles / Projectiles / Tour
  , Direction(..), dirVector
  , Cadence(..), prop_inv_cadence, mkCadence, tickCadence
  , prop_pre_tickCadence, prop_post_tickCadence
  , Obstacle(..), prop_inv_obstacle, mkObstacle, defileObstacle
  , ProjectileOwner(..)
  , Projectile(..), prop_inv_projectile, mkProjectile
  , avanceProjectile, prop_pre_avanceProjectile, prop_post_avanceProjectile
  , finDeTourProjectile, prop_pre_finDeTourProjectile, prop_post_finDeTourProjectile
  , finDeTourObstacles, prop_pre_finDeTourObstacles, prop_post_finDeTourObstacles

  -- Partie 5 : Ennemis
  , Action(..)
  , Oracle(..), oracleStep, prop_pre_oracleStep, prop_post_oracleStep
  , PV(..), prop_inv_pv, mkPV
  , Ennemi(..), prop_inv_ennemi, mkEnnemi
  , finDeTourEnnemi, prop_pre_finDeTourEnnemi, prop_post_finDeTourEnnemi
  ) where

import Data.Text (Text)
import qualified Data.Text as T

someFunc :: IO ()
someFunc = putStrLn "someFunc"

tshow :: Show a => a -> Text
tshow = T.pack . show

--------------------------------------------------------------------------------
-- Partie 1 : Hitbox
--------------------------------------------------------------------------------

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
prop_inv_hitbox (MurGauche ls) = length ls >= 2
prop_inv_hitbox (MurDroit ls) = length ls >= 2

-- Smart constructors
mkPoint :: Int -> Int -> Hitbox
mkPoint x y = Point x y

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

-- appropriate_segment (Point x y) [(Int, Int)] : Trouve les deux points (le segment) entre lesquels les coordonnées de notre Point x y sont.
point_appropriate_segment :: Hitbox -> [(Int, Int)] -> ((Int,Int), (Int, Int))
point_appropriate_segment p@(Point _ y) (p1@(_, y1) : p2@(_, y2) : ls') = 
  if y >= y1 && y <= y2 then (p1, p2)
  else point_appropriate_segment  p (p2 : ls')
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

collision p@(Point x y) (MurGauche ls) = collision' $ point_appropriate_segment p ls
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
collision p@(Point x y) (MurDroit ls) = collision' $ point_appropriate_segment p ls
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


--------------------------------------------------------------------------------
-- Partie 2 : Murs
--------------------------------------------------------------------------------

mur_gauche_dents_scie :: Int -> Int -> Hitbox
mur_gauche_dents_scie dx dy = MurGauche $ (0,0) : aux 0 0 where
    aux prevX prevY = 
      let 
        (newX, newY) = 
          if prevX == 0 then (dx, prevY + dy)
          else (0, prevY + dy)
      in
      (newX, newY) : aux newX newY

mur_gauche_vertical :: Int -> Int -> Hitbox
mur_gauche_vertical x dy = MurGauche (aux 0)
  where
    aux y = (x, y) : aux (y + dy)

-- Question 2.1:
mur_dent_scie_Exam :: Hitbox
mur_dent_scie_Exam = mur_gauche_dents_scie 5 10

-- Question 2.2:
mur_gauche_vertical_10 :: Hitbox
mur_gauche_vertical_10 = mur_gauche_vertical 10 10


--------------------------------------------------------------------------------
-- Partie 3 : Obstacles / Projectiles / Tour
--------------------------------------------------------------------------------

data Direction = Haut | Bas | Gauche | Droite
  deriving (Eq, Show, Enum, Bounded)

dirVector :: Direction -> (Int, Int)
dirVector Haut   = (0, 1)
dirVector Bas    = (0, -1)
dirVector Gauche = (-1, 0)
dirVector Droite = (1, 0)

-- Cadence: attente>0, restant ∈ [0..attente-1], restant==0 => bouge maintenant
-- attente = N (tous les combien de tours on bouge)
--restant = compteur interne “combien de tours avant le prochain mouvement”
data Cadence = Cadence { attente :: Int, restant :: Int }
  deriving (Eq, Show)

prop_inv_cadence :: Cadence -> Bool
prop_inv_cadence (Cadence a r) = a > 0 && 0 <= r && r < a

mkCadence :: Int -> Either Text Cadence
mkCadence a
  | a > 0     = Right (Cadence a 0)
  | otherwise = Left ("attente doit être > 0, reçu " <> tshow a)

-- si restant == 0:
-- on doit bouger ce tour-ci (True)
-- puis on remet le compteur à a-1 (il faudra attendre a-1 tours avant le prochain mouvement)
tickCadence :: Cadence -> (Bool, Cadence)
tickCadence c@(Cadence a r)
  | not (prop_inv_cadence c) = error "tickCadence: invariant cadence violé"
  | r == 0    = (True,  Cadence a (a - 1))
  | otherwise = (False, Cadence a (r - 1))

prop_pre_tickCadence :: Cadence -> Bool
prop_pre_tickCadence = prop_inv_cadence

prop_post_tickCadence :: Cadence -> (Bool, Cadence) -> Bool
prop_post_tickCadence (Cadence a r) (moveNow, Cadence a' r') =
  a' == a
  && prop_inv_cadence (Cadence a' r')
  && if r == 0
       then moveNow && r' == a - 1
       else (not moveNow) && r' == r - 1

translateHitbox :: Int -> Int -> Hitbox -> Hitbox
translateHitbox dx dy (Point x y)            = Point (x + dx) (y + dy)
translateHitbox dx dy (Disque xc yc r)       = Disque (xc + dx) (yc + dy) r
translateHitbox dx dy (Rectangle x y w h)    = Rectangle (x + dx) (y + dy) w h
translateHitbox dx dy (Composee hs)          = Composee (map (translateHitbox dx dy) hs)

newtype Obstacle = Obstacle { obsHitbox :: Hitbox }
  deriving (Eq, Show)

prop_inv_obstacle :: Obstacle -> Bool
prop_inv_obstacle (Obstacle h) = prop_inv_hitbox h

mkObstacle :: Hitbox -> Either Text Obstacle
mkObstacle h
  | prop_inv_hitbox h = Right (Obstacle h)
  | otherwise         = Left "Hitbox invalide pour obstacle"

defileObstacle :: Obstacle -> Obstacle
defileObstacle (Obstacle h) = Obstacle (translateHitbox 0 (-1) h)

data ProjectileOwner = TirJoueuse | TirEnnemi
  deriving (Eq, Show, Enum, Bounded)

data Projectile = Projectile
  { prHitbox  :: Hitbox
  , prDir     :: Direction
  , prCadence :: Cadence
  , prOwner   :: ProjectileOwner
  } deriving (Eq, Show)

prop_inv_projectile :: Projectile -> Bool
prop_inv_projectile p =
  prop_inv_hitbox (prHitbox p) && prop_inv_cadence (prCadence p)

mkProjectile :: Hitbox -> Direction -> Cadence -> ProjectileOwner -> Either Text Projectile
mkProjectile h d c o
  | prop_inv_hitbox h && prop_inv_cadence c = Right (Projectile h d c o)
  | otherwise = Left "Projectile invalide (hitbox ou cadence)"

avanceProjectile :: Projectile -> Projectile
avanceProjectile p =
  let (dx, dy) = dirVector (prDir p)
  in p { prHitbox = translateHitbox dx dy (prHitbox p) }

prop_pre_avanceProjectile :: Projectile -> Bool
prop_pre_avanceProjectile = prop_inv_projectile

prop_post_avanceProjectile :: Projectile -> Projectile -> Bool
prop_post_avanceProjectile p p' =
  prop_inv_projectile p'
  && prDir p' == prDir p
  && prCadence p' == prCadence p
  && prOwner p' == prOwner p

finDeTourProjectile :: Projectile -> Projectile
finDeTourProjectile p
  | not (prop_inv_projectile p) = error "finDeTourProjectile: invariant projectile violé"
  | otherwise =
      let (moveNow, c') = tickCadence (prCadence p)
          p' = p { prCadence = c' }
      in if moveNow then avanceProjectile p' else p'

prop_pre_finDeTourProjectile :: Projectile -> Bool
prop_pre_finDeTourProjectile = prop_inv_projectile

prop_post_finDeTourProjectile :: Projectile -> Projectile -> Bool
prop_post_finDeTourProjectile p p' =
  prop_inv_projectile p'
  && prDir p' == prDir p
  && prOwner p' == prOwner p

-- scrolling global: on tick une cadence, si moveNow alors tous les obstacles descendent
finDeTourObstacles :: Cadence -> [Obstacle] -> (Cadence, [Obstacle])
finDeTourObstacles cad obs
  | not (prop_inv_cadence cad) = error "finDeTourObstacles: invariant cadence violé"
  | otherwise =
      let (moveNow, cad') = tickCadence cad
          obs' = if moveNow then map defileObstacle obs else obs
      in (cad', obs')

prop_pre_finDeTourObstacles :: Cadence -> [Obstacle] -> Bool
prop_pre_finDeTourObstacles cad obs =
  prop_inv_cadence cad && all prop_inv_obstacle obs

prop_post_finDeTourObstacles :: Cadence -> [Obstacle] -> (Cadence, [Obstacle]) -> Bool
prop_post_finDeTourObstacles _ _ (cad', obs') =
  prop_inv_cadence cad' && all prop_inv_obstacle obs'

--------------------------------------------------------------------------------
-- Partie 5 : Ennemis
--------------------------------------------------------------------------------

data Action = Deplacer Direction | Tirer | Attendre
  deriving (Eq, Show)

data Oracle
  = Scripted [Action] Int     -- liste cyclique + index
  | FromInt                   -- action déterminée par l'int du moteur
  deriving (Eq, Show)

prop_inv_oracle :: Oracle -> Bool
prop_inv_oracle (Scripted as i) = not (null as) && i >= 0
prop_inv_oracle FromInt         = True

oracleStep :: Oracle -> Int -> (Action, Oracle)
oracleStep o n
  | not (prop_inv_oracle o) = error "oracleStep: invariant oracle violé"
oracleStep (Scripted as i) _ =
  let k = i `mod` length as
  in (as !! k, Scripted as (i + 1))
oracleStep FromInt n = (actionFromInt n, FromInt)

prop_pre_oracleStep :: Oracle -> Int -> Bool
prop_pre_oracleStep o _ = prop_inv_oracle o

prop_post_oracleStep :: Oracle -> Int -> (Action, Oracle) -> Bool
prop_post_oracleStep _ _ (_a, o') = prop_inv_oracle o'

actionFromInt :: Int -> Action
actionFromInt n =
  case abs n `mod` 5 of
    0 -> Attendre
    1 -> Tirer
    2 -> Deplacer Gauche
    3 -> Deplacer Droite
    _ -> Deplacer Bas

newtype PV = PV Int
  deriving (Eq, Show, Ord)

prop_inv_pv :: PV -> Bool
prop_inv_pv (PV x) = x > 0

mkPV :: Int -> Either Text PV
mkPV x
  | x > 0     = Right (PV x)
  | otherwise = Left ("PV doit être > 0, reçu " <> tshow x)

data Ennemi = Ennemi
  { eHitbox   :: Hitbox
  , ePV       :: PV
  , eOracle   :: Oracle
  , eCadTir   :: Cadence
  } deriving (Eq, Show)

prop_inv_ennemi :: Ennemi -> Bool
prop_inv_ennemi e =
  prop_inv_hitbox (eHitbox e)
  && prop_inv_pv (ePV e)
  && prop_inv_oracle (eOracle e)
  && prop_inv_cadence (eCadTir e)

mkEnnemi :: Hitbox -> PV -> Oracle -> Cadence -> Either Text Ennemi
mkEnnemi h pv o cad
  | prop_inv_hitbox h && prop_inv_pv pv && prop_inv_oracle o && prop_inv_cadence cad
      = Right (Ennemi h pv o cad)
  | otherwise = Left "Ennemi invalide (hitbox/PV/oracle/cadence)"

-- fin de tour ennemi: renvoie (ennemi mis à jour, projectile éventuellement créé)
finDeTourEnnemi :: Int -> Ennemi -> (Ennemi, Maybe Projectile)
finDeTourEnnemi rnd e
  | not (prop_inv_ennemi e) = error "finDeTourEnnemi: invariant ennemi violé"
  | otherwise =
      let (act, o') = oracleStep (eOracle e) rnd
          e0 = e { eOracle = o' }
      in case act of
          Attendre -> (e0, Nothing)
          Deplacer d ->
            let (dx,dy) = dirVector d
            in (e0 { eHitbox = translateHitbox dx dy (eHitbox e0) }, Nothing)
          Tirer ->
            let (shootNow, cad') = tickCadence (eCadTir e0)
                e1 = e0 { eCadTir = cad' }
            in if shootNow
                 then
                   let hShot = eHitbox e1
                       p = Projectile hShot Bas (Cadence 1 0) TirEnnemi
                   in (e1, Just p)
                 else (e1, Nothing)

prop_pre_finDeTourEnnemi :: Int -> Ennemi -> Bool
prop_pre_finDeTourEnnemi _ = prop_inv_ennemi

prop_post_finDeTourEnnemi :: Int -> Ennemi -> (Ennemi, Maybe Projectile) -> Bool
prop_post_finDeTourEnnemi _ _ (e', mp) =
  prop_inv_ennemi e' && maybe True prop_inv_projectile mp