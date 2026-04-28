{-# LANGUAGE OverloadedStrings #-}

module Model.Engine.EnnemiAI where

import Model.Engine.Types
import Model.Hitbox
import Model.Objects

---------------------------------------------------------------------------------
-- Comportement simple des ennemis
---------------------------------------------------------------------------------

-- | Fait évoluer un ennemi avec une logique simple :
--   - il se rapproche horizontalement de la joueuse la plus proche ;
--   - il descend parfois ;
--   - il tire selon sa cadence.
finDeTourEnnemiIntelligent
  :: Int
  -> [VaisseauJoueuse]
  -> Ennemi
  -> (Ennemi, Maybe Projectile)
finDeTourEnnemiIntelligent graine joueuses e
  | not (prop_inv_ennemi e) =
      error "finDeTourEnnemiIntelligent: invariant ennemi violé"
  | otherwise =
      let e1 = rapprocherEnnemiDesJoueuses joueuses e
          e2 = descendreEnnemiParfois graine e1
      in tirerEnnemiSiPret e2


rapprocherEnnemiDesJoueuses :: [VaisseauJoueuse] -> Ennemi -> Ennemi
rapprocherEnnemiDesJoueuses joueuses e =
  case joueuseCible e joueuses of
    Nothing ->
      e

    Just cible ->
      case directionVersCible e cible of
        Nothing -> e
        Just d  -> deplacerEnnemi d e

descendreEnnemiParfois :: Int -> Ennemi -> Ennemi
descendreEnnemiParfois graine e
  | abs graine `mod` 6 == 0 = deplacerEnnemi Bas e
  | otherwise               = e

deplacerEnnemi :: Direction -> Ennemi -> Ennemi
deplacerEnnemi d e =
  let (dx, dy) = dirVector d
  in e { eHitbox = translateHitbox dx dy (eHitbox e) }

directionVersCible :: Ennemi -> VaisseauJoueuse -> Maybe Direction
directionVersCible e v
  | ecx < vcx - margeAlignement = Just Droite
  | ecx > vcx + margeAlignement = Just Gauche
  | otherwise                   = Nothing
  where
    (ecx, _) = centreHitbox (eHitbox e)
    (vcx, _) = centreHitbox (vjHitbox v)

margeAlignement :: Int
margeAlignement = 8

joueuseCible :: Ennemi -> [VaisseauJoueuse] -> Maybe VaisseauJoueuse
joueuseCible _ [] =
  Nothing

joueuseCible e joueuses =
  case filter joueuseActive joueuses of
    []     -> Nothing
    (j:js) -> Just (foldl (plusProcheHorizontalement e) j js)

joueuseActive :: VaisseauJoueuse -> Bool
joueuseActive v =
  vjPv v > 0 && vjEssais v > 0

plusProcheHorizontalement
  :: Ennemi
  -> VaisseauJoueuse
  -> VaisseauJoueuse
  -> VaisseauJoueuse
plusProcheHorizontalement e a b
  | distanceHorizontale e a <= distanceHorizontale e b = a
  | otherwise                                          = b

distanceHorizontale :: Ennemi -> VaisseauJoueuse -> Int
distanceHorizontale e v =
  abs (ex - vx)
  where
    (ex, _) = centreHitbox (eHitbox e)
    (vx, _) = centreHitbox (vjHitbox v)

centreHitbox :: Hitbox -> (Int, Int)
centreHitbox (Point x y) =
  (x, y)

centreHitbox (Disque x y _) =
  (x, y)

centreHitbox (Rectangle x y w h) =
  (x + w `div` 2, y + h `div` 2)

centreHitbox (Composee (h:_)) =
  centreHitbox h

centreHitbox (Composee []) =
  (0, 0)

centreHitbox (MurGauche _) =
  (0, 0)

centreHitbox (MurDroit _) =
  (largeurZoneJeu, 0)


tirerEnnemiSiPret :: Ennemi -> (Ennemi, Maybe Projectile)
tirerEnnemiSiPret e =
  let (tirMaintenant, cad') = tickCadence (eCadTir e)
      e' = e { eCadTir = cad' }
  in if tirMaintenant
       then (e', Just (projectileEnnemi e'))
       else (e', Nothing)

projectileEnnemi :: Ennemi -> Projectile
projectileEnnemi e =
  Projectile
    { prHitbox  = pointTirEnnemi (eHitbox e)
    , prDir     = Bas
    , prCadence = Cadence 1 0
    , prOwner   = TirEnnemi
    }

pointTirEnnemi :: Hitbox -> Hitbox
pointTirEnnemi h =
  let (x, y) = centreBasHitbox h
  in Point x y

centreBasHitbox :: Hitbox -> (Int, Int)
centreBasHitbox (Point x y) =
  (x, y)

centreBasHitbox (Disque x y r) =
  (x, y - r)

centreBasHitbox (Rectangle x y w _) =
  (x + w `div` 2, y)

centreBasHitbox (Composee (h:_)) =
  centreBasHitbox h

centreBasHitbox (Composee []) =
  (0, 0)

centreBasHitbox (MurGauche _) =
  (0, 0)

centreBasHitbox (MurDroit _) =
  (largeurZoneJeu, 0)

prop_pre_finDeTourEnnemiIntelligent :: Int -> [VaisseauJoueuse] -> Ennemi -> Bool
prop_pre_finDeTourEnnemiIntelligent _ joueuses e =
  all prop_inv_vaisseau joueuses && prop_inv_ennemi e

prop_post_finDeTourEnnemiIntelligent
  :: Int
  -> [VaisseauJoueuse]
  -> Ennemi
  -> (Ennemi, Maybe Projectile)
  -> Bool
prop_post_finDeTourEnnemiIntelligent _ _ _ (e', mp) =
  prop_inv_ennemi e' && maybe True prop_inv_projectile mp