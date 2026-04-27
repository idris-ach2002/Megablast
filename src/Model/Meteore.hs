{-# LANGUAGE OverloadedStrings #-}

module Model.Meteore where

import Data.Text (Text)
import Model.Hitbox
import Model.Objects

-- | Un météore est un obstacle mobile qui descend verticalement.
--   Il possède une hitbox, une cadence de déplacement et un nombre de dégâts.
data Meteore = Meteore
  { mtHitbox  :: Hitbox
  , mtCadence :: Cadence
  , mtDegats  :: Int
  } deriving (Eq, Show)

prop_inv_meteore :: Meteore -> Bool
prop_inv_meteore mt =
     prop_inv_hitbox (mtHitbox mt)
  && prop_inv_cadence (mtCadence mt)
  && mtDegats mt > 0

mkMeteore :: Hitbox -> Cadence -> Int -> Either Text Meteore
mkMeteore h cad degats
  | not (prop_inv_hitbox h)    = Left "Meteore: hitbox invalide"
  | not (prop_inv_cadence cad) = Left "Meteore: cadence invalide"
  | degats <= 0                = Left "Meteore: degats non strictement positifs"
  | otherwise                  = Right (Meteore h cad degats)

-- | Constructeur pratique pour un météore circulaire.
mkMeteoreRond :: Int -> Int -> Int -> Cadence -> Either Text Meteore
mkMeteoreRond x y rayon cad = do
  h <- mkDisque x y rayon
  mkMeteore h cad 1

avanceMeteore :: Meteore -> Meteore
avanceMeteore mt =
  mt { mtHitbox = translateHitbox 0 (-1) (mtHitbox mt) }

finDeTourMeteore :: Meteore -> Meteore
finDeTourMeteore mt
  | not (prop_inv_meteore mt) = error "finDeTourMeteore: invariant meteore viole"
  | otherwise =
      let (moveNow, cad') = tickCadence (mtCadence mt)
          mt' = mt { mtCadence = cad' }
      in if moveNow then avanceMeteore mt' else mt'

prop_pre_finDeTourMeteore :: Meteore -> Bool
prop_pre_finDeTourMeteore = prop_inv_meteore

prop_post_finDeTourMeteore :: Meteore -> Meteore -> Bool
prop_post_finDeTourMeteore mt mt' =
     prop_inv_meteore mt'
  && mtDegats mt' == mtDegats mt
