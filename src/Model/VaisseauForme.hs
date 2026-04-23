{-# LANGUAGE OverloadedStrings #-}

module Model.VaisseauForme where

import Data.Text (Text)
import Model.Hitbox

-- | Décomposition sémantique d'un vaisseau.
--   Les collisions continuent de se faire sur la hitbox globale,
--   mais la vue et certaines opérations métier peuvent raisonner
--   sur les parties nommées.
data PartiesVaisseau = PartiesVaisseau
  { pvCorps     :: Hitbox
  , pvReacteurG :: Hitbox
  , pvReacteurD :: Hitbox
  , pvCockpit   :: Maybe Hitbox
  } deriving (Eq, Show)

estRectangle :: Hitbox -> Bool
estRectangle (Rectangle _ _ _ _) = True
estRectangle _                   = False

estDisque :: Hitbox -> Bool
estDisque (Disque _ _ _) = True
estDisque _              = False

prop_inv_partiesVaisseau :: PartiesVaisseau -> Bool
prop_inv_partiesVaisseau pv =
     estRectangle (pvCorps pv)
  && estDisque    (pvReacteurG pv)
  && estDisque    (pvReacteurD pv)
  && prop_inv_hitbox (pvCorps pv)
  && prop_inv_hitbox (pvReacteurG pv)
  && prop_inv_hitbox (pvReacteurD pv)
  && maybe True prop_inv_hitbox (pvCockpit pv)

mkPartiesVaisseau :: Hitbox -> Hitbox -> Hitbox -> Maybe Hitbox -> Either Text PartiesVaisseau
mkPartiesVaisseau corps reacteurG reacteurD cockpit
  | not (estRectangle corps) = Left "PartiesVaisseau: le corps doit etre un Rectangle"
  | not (prop_inv_hitbox corps) = Left "PartiesVaisseau: corps invalide"
  | not (estDisque reacteurG) = Left "PartiesVaisseau: le reacteur gauche doit etre un Disque"
  | not (prop_inv_hitbox reacteurG) = Left "PartiesVaisseau: reacteur gauche invalide"
  | not (estDisque reacteurD) = Left "PartiesVaisseau: le reacteur droit doit etre un Disque"
  | not (prop_inv_hitbox reacteurD) = Left "PartiesVaisseau: reacteur droit invalide"
  | not (maybe True prop_inv_hitbox cockpit) = Left "PartiesVaisseau: cockpit invalide"
  | otherwise = Right (PartiesVaisseau corps reacteurG reacteurD cockpit)

translatePartiesVaisseau :: Int -> Int -> PartiesVaisseau -> PartiesVaisseau
translatePartiesVaisseau dx dy pv =
  PartiesVaisseau
    { pvCorps     = translateHitboxParties dx dy (pvCorps pv)
    , pvReacteurG = translateHitboxParties dx dy (pvReacteurG pv)
    , pvReacteurD = translateHitboxParties dx dy (pvReacteurD pv)
    , pvCockpit   = fmap (translateHitboxParties dx dy) (pvCockpit pv)
    }

hitboxPartiesVaisseau :: PartiesVaisseau -> Hitbox
hitboxPartiesVaisseau pv =
  Composee $
       [pvCorps pv, pvReacteurG pv, pvReacteurD pv]
    ++ maybe [] pure (pvCockpit pv)

-- | Petite hitbox utilisée pour faire partir les projectiles.
--   On privilégie le cockpit s'il existe, sinon le haut-centre du corps
hitboxTirPartiesVaisseau :: PartiesVaisseau -> Hitbox
hitboxTirPartiesVaisseau pv =
  maybe (pointHautCentre (pvCorps pv)) pointHautCentre (pvCockpit pv)

prop_pre_translatePartiesVaisseau :: PartiesVaisseau -> Bool
prop_pre_translatePartiesVaisseau = prop_inv_partiesVaisseau

prop_post_translatePartiesVaisseau :: PartiesVaisseau -> PartiesVaisseau -> Bool
prop_post_translatePartiesVaisseau _ pv' = prop_inv_partiesVaisseau pv'

prop_pre_hitboxPartiesVaisseau :: PartiesVaisseau -> Bool
prop_pre_hitboxPartiesVaisseau = prop_inv_partiesVaisseau

prop_post_hitboxPartiesVaisseau :: PartiesVaisseau -> Hitbox -> Bool
prop_post_hitboxPartiesVaisseau _ h = prop_inv_hitbox h

-- | Forme standard simple : corps rectangulaire, deux réacteurs ronds,
--   cockpit centré. Les coordonnées (x,y) désignent le coin bas-gauche du
--   volume englobant du vaisseau.
mkPartiesVaisseauStandard :: Int -> Int -> Either Text PartiesVaisseau
mkPartiesVaisseauStandard x y = do
  -- TODO: stocker dans des constantes au lieu d'écrire en dur.
  corps     <- mkRectangle x (y + 8) 24 28
  reacteurG <- mkDisque (x + 6) (y + 6) 6
  reacteurD <- mkDisque (x + 18) (y + 6) 6
  cockpit   <- mkDisque (x + 12) (y + 28) 4
  mkPartiesVaisseau corps reacteurG reacteurD (Just cockpit)

translateHitboxParties :: Int -> Int -> Hitbox -> Hitbox
translateHitboxParties dx dy (Point x y)         = Point (x + dx) (y + dy)
translateHitboxParties dx dy (Disque xc yc r)    = Disque (xc + dx) (yc + dy) r
translateHitboxParties dx dy (Rectangle x y w h) = Rectangle (x + dx) (y + dy) w h
translateHitboxParties dx dy (MurGauche pts)     = MurGauche (map (transPoint dx dy) pts)
translateHitboxParties dx dy (MurDroit pts)      = MurDroit (map (transPoint dx dy) pts)
translateHitboxParties dx dy (Composee hs)       = Composee (map (translateHitboxParties dx dy) hs)

transPoint :: Int -> Int -> (Int, Int) -> (Int, Int)
transPoint dx dy (x, y) = (x + dx, y + dy)

pointHautCentre :: Hitbox -> Hitbox
pointHautCentre (Rectangle x y w h) = Point (x + w `div` 2) (y + h)
pointHautCentre (Disque xc yc r)    = Point xc (yc + r)
pointHautCentre (Point x y)         = Point x y
pointHautCentre (Composee (h:_))    = pointHautCentre h
pointHautCentre (Composee [])       = Point 0 0
pointHautCentre (MurGauche ((x,y):_)) = Point x y
pointHautCentre (MurGauche [])        = Point 0 0
pointHautCentre (MurDroit ((x,y):_))  = Point x y
pointHautCentre (MurDroit [])         = Point 0 0
