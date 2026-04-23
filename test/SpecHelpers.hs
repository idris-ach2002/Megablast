{-# OPTIONS_GHC -Wno-orphans #-}

module SpecHelpers where

import Engine
import Hitbox
import Objects
import Test.QuickCheck

-- Ça sert à contraindre les valeurs générées (ici: petites valeurs, pour éviter
-- des trucs énormes et garder des contre-exemples lisibles).
newtype SmallInt = SmallInt Int deriving (Show)

instance Arbitrary SmallInt where
  arbitrary = SmallInt <$> chooseInt (-100, 100)

isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft _        = False

isRightWith :: (b -> Bool) -> Either a b -> Bool
isRightWith pr (Right x) = pr x
isRightWith _  _         = False

instance Arbitrary Direction where
  arbitrary = elements [minBound .. maxBound]

instance Arbitrary ProjectileOwner where
  arbitrary = elements [minBound .. maxBound]

-- | Générateur récursif de hitbox générales, utile pour les tests Hitbox.
-- On borne la profondeur pour éviter des Composee infiniment imbriquées.
genHitbox :: Int -> Gen Hitbox
genHitbox n
  | n <= 0 = base
  | otherwise = frequency
      [ (6, base)
      , (1, composee)
      ]
  where
    base = oneof
      [ Point <$> chooseInt (-200,200) <*> chooseInt (-200,200)
      , do xc <- chooseInt (-200,200)
           yc <- chooseInt (-200,200)
           Positive r <- arbitrary
           pure (Disque xc yc r)
      , do x <- chooseInt (-200,200)
           y <- chooseInt (-200,200)
           Positive w <- arbitrary
           Positive h <- arbitrary
           pure (Rectangle x y w h)
      ]
    composee = do
      k <- chooseInt (2,4)
      hs <- vectorOf k (genHitbox (n `div` 2))
      pure (Composee hs)

instance Arbitrary Hitbox where
  arbitrary = sized genHitbox

genCadence :: Gen Cadence
genCadence = do
  Positive a <- arbitrary
  r <- chooseInt (0, a - 1)
  pure (Cadence a r)

genProjectile :: Gen Projectile
genProjectile = do
  h <- arbitrary
  d <- arbitrary
  c <- genCadence
  o <- arbitrary
  pure (Projectile h d c o)

genVaisseau :: Gen VaisseauJoueuse
genVaisseau = do
  h <- arbitrary
  pv <- chooseInt (0, 100)
  essais <- chooseInt (0, 10)
  cad <- genCadence
  pure (VaisseauJoueuse h pv essais cad)

genEnnemi :: Gen Ennemi
genEnnemi = do
  h <- arbitrary
  Positive pv <- arbitrary
  cad <- genCadence
  let o = Scripted [Attendre, Tirer, Deplacer Gauche, Deplacer Droite] 0
  pure (Ennemi h (PV pv) o cad)

--------------------------------------------------------------------------------
-- Générateurs simples pour le moteur
--
-- On choisit volontairement des hitbox atomiques bien supportées par le moteur
-- actuel afin d'éviter de tomber sur des couples de collision encore non gérés.
--------------------------------------------------------------------------------

genRectHitbox :: Gen Hitbox
genRectHitbox = do
  x <- chooseInt (0, largeurZoneJeu `div` 2)
  y <- chooseInt (0, hauteurZoneJeu `div` 2)
  w <- chooseInt (1, 30)
  h <- chooseInt (1, 30)
  pure (Rectangle x y w h)

genPointHitbox :: Gen Hitbox
genPointHitbox = do
  x <- chooseInt (0, largeurZoneJeu)
  y <- chooseInt (0, hauteurZoneJeu)
  pure (Point x y)

-- | Génère une joueuse active, c'est-à-dire avec au moins un PV ou un essai.
genVaisseauActif :: Gen VaisseauJoueuse
genVaisseauActif = do
  h <- genRectHitbox
  pv <- chooseInt (0, 5)
  essais <- chooseInt (0, 3)
  cad <- genCadence
  let pv' = if pv == 0 && essais == 0 then 1 else pv
  pure (VaisseauJoueuse h pv' essais cad)

genObstacleSimple :: Gen Obstacle
genObstacleSimple = Obstacle <$> genRectHitbox

genProjectileSimple :: Gen Projectile
genProjectileSimple = do
  h <- genPointHitbox
  d <- arbitrary
  c <- genCadence
  o <- arbitrary
  pure (Projectile h d c o)

genEnnemiSimple :: Gen Ennemi
genEnnemiSimple = do
  h <- genRectHitbox
  pv <- chooseInt (1, 5)
  cad <- genCadence
  let o = Scripted [Attendre, Tirer, Deplacer Gauche, Deplacer Droite] 0
  pure (Ennemi h (PV pv) o cad)

genEvenementPlanifieSimple :: Gen EvenementPlanifie
genEvenementPlanifieSimple = do
  tour <- chooseInt (0, 20)
  evt <- oneof
    [ AppEnnemi <$> genEnnemiSimple
    , AppObstacle <$> genObstacleSimple
    , AppProjectile <$> genProjectileSimple
    , DisparEnnemi <$> chooseInt (0, 5)
    , DisparObstacle <$> chooseInt (0, 5)
    ]
  pure (EvenementPlanifie tour evt)

-- | Générateur simple de moteur valide.
-- On garde des rectangles/points pour rester dans la partie de collision déjà
-- couverte par l'implémentation actuelle.
genMoteur :: Gen Moteur
genMoteur = do
  nbObs <- chooseInt (0, 4)
  nbProj <- chooseInt (0, 5)
  nbEnn <- chooseInt (0, 4)
  nbJ <- chooseInt (1, 2)
  obs <- vectorOf nbObs genObstacleSimple
  projs <- vectorOf nbProj genProjectileSimple
  enns <- vectorOf nbEnn genEnnemiSimple
  jous <- vectorOf nbJ genVaisseauActif
  cad <- genCadence
  evts <- listOf genEvenementPlanifieSimple
  tour <- chooseInt (0, 20)
  seed <- arbitrary
  case mkMoteur obs projs enns jous cad evts tour seed of
    Right m -> pure m
    Left _  -> genMoteur

instance Arbitrary Moteur where
  arbitrary = genMoteur
