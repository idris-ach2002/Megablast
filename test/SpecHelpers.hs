{-# OPTIONS_GHC -Wno-orphans #-}

module SpecHelpers where

import Data.Text (Text)
import Model.Engine
import Model.Hitbox
import Model.Objects
import Model.VaisseauForme
import Test.QuickCheck

newtype SmallInt = SmallInt Int deriving (Show)

instance Arbitrary SmallInt where
  arbitrary = SmallInt <$> chooseInt (-100, 100)

isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft _        = False

isRightWith :: (b -> Bool) -> Either a b -> Bool
isRightWith pr (Right x) = pr x
isRightWith _  _         = False

unsafeRight :: String -> Either e a -> a
unsafeRight _   (Right x) = x
unsafeRight msg (Left _)  = error msg

instance Arbitrary Direction where
  arbitrary = elements [minBound .. maxBound]

instance Arbitrary ProjectileOwner where
  arbitrary = elements [minBound .. maxBound]

genHitbox :: Int -> Gen Hitbox
genHitbox n
  | n <= 0 = base
  | otherwise = frequency
      [ (6, base)
      , (1, composee)
      ]
  where
    base = oneof
      [ Point <$> chooseInt (-200, 200) <*> chooseInt (-200, 200)
      , do
          xc <- chooseInt (-200, 200)
          yc <- chooseInt (-200, 200)
          Positive r <- arbitrary
          pure (Disque xc yc r)
      , do
          x <- chooseInt (-200, 200)
          y <- chooseInt (-200, 200)
          Positive w <- arbitrary
          Positive h <- arbitrary
          pure (Rectangle x y w h)
      ]

    composee = do
      k <- chooseInt (2, 4)
      hs <- vectorOf k (genHitbox (n `div` 2))
      pure (Composee hs)

instance Arbitrary Hitbox where
  arbitrary = sized genHitbox

genCadence :: Gen Cadence
genCadence = do
  Positive a <- arbitrary
  r <- chooseInt (0, a - 1)
  pure (Cadence a r)

genPartiesVaisseau :: Gen PartiesVaisseau
genPartiesVaisseau = do
  x <- chooseInt (50, largeurZoneJeu - 100)
  y <- chooseInt (20, hauteurZoneJeu - 100)
  pure $
    unsafeRight
      "genPartiesVaisseau: mkPartiesVaisseauStandard impossible"
      (mkPartiesVaisseauStandard x y)

genProjectile :: Gen Projectile
genProjectile = do
  h <- arbitrary
  d <- arbitrary
  c <- genCadence
  o <- arbitrary
  pure (Projectile h d c o)

genVaisseau :: Gen VaisseauJoueuse
genVaisseau = do
  forme <- genPartiesVaisseau
  pv <- chooseInt (0, 100)
  essais <- chooseInt (0, 10)
  cad <- genCadence
  pure (VaisseauJoueuse forme pv essais cad)

genEnnemi :: Gen Ennemi
genEnnemi = do
  h <- arbitrary
  Positive pv <- arbitrary
  cad <- genCadence
  let o = Scripted [Attendre, Tirer, Deplacer Gauche, Deplacer Droite] 0
  pure (Ennemi h (PV pv) o cad)

mursTest :: MursNiveau
mursTest =
  unsafeRight "mursTest invalide" $
    mkMursNiveau
      (MurGauche [(0, 0), (0, hauteurZoneJeu + 200)])
      (MurDroit  [(largeurZoneJeu, 0), (largeurZoneJeu, hauteurZoneJeu + 200)])

mkMoteurTest
  :: [Obstacle]
  -> [Projectile]
  -> [Ennemi]
  -> [VaisseauJoueuse]
  -> Cadence
  -> [EvenementPlanifie]
  -> Int
  -> Int
  -> Either Text Moteur
mkMoteurTest obs projs enns jous cad evts tour seed =
  mkMoteur obs projs enns jous mursTest cad evts tour seed

genRectHitbox :: Gen Hitbox
genRectHitbox = do
  x <- chooseInt (50, largeurZoneJeu - 100)
  y <- chooseInt (20, hauteurZoneJeu - 100)
  w <- chooseInt (1, 30)
  h <- chooseInt (1, 30)
  pure (Rectangle x y w h)

genPointHitbox :: Gen Hitbox
genPointHitbox = do
  x <- chooseInt (0, largeurZoneJeu)
  y <- chooseInt (0, hauteurZoneJeu)
  pure (Point x y)

genVaisseauActif :: Gen VaisseauJoueuse
genVaisseauActif = do
  forme <- genPartiesVaisseau
  pv <- chooseInt (1, 5)
  essais <- chooseInt (0, 3)
  cad <- genCadence
  pure (VaisseauJoueuse forme pv essais cad)

genObstacleSimple :: Gen Obstacle
genObstacleSimple =
  Obstacle <$> genRectHitbox

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

  case mkMoteurTest obs projs enns jous cad evts tour seed of
    Right m -> pure m
    Left _  -> genMoteur

instance Arbitrary Moteur where
  arbitrary = genMoteur