{-# OPTIONS_GHC -Wno-orphans #-}

module SpecHelpers where

import Data.Text (Text)
import Model.Engine
import Model.Hitbox
import Model.Meteore
import Model.Objects
import Model.Score
import Model.VaisseauForme
import Test.QuickCheck

newtype SmallInt = SmallInt Int deriving (Show)

instance Arbitrary SmallInt where
  arbitrary = SmallInt <$> chooseInt (-100, 100)

isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft _        = False

isRight :: Either a b -> Bool
isRight (Right _) = True
isRight _         = False

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

instance Arbitrary Action where
  arbitrary = oneof
    [ pure Attendre
    , pure Tirer
    , Deplacer <$> arbitrary
    ]

smallPositive :: Gen Int
smallPositive = chooseInt (1, 40)

genPointCoords :: Gen (Int, Int)
genPointCoords =
  (,) <$> chooseInt (-200, 1000) <*> chooseInt (-200, 1100)

genPointHitbox :: Gen Hitbox
genPointHitbox = uncurry Point <$> genPointCoords

genDisqueHitbox :: Gen Hitbox
genDisqueHitbox = do
  (x, y) <- genPointCoords
  r <- chooseInt (1, 40)
  pure (Disque x y r)

genRectHitbox :: Gen Hitbox
genRectHitbox = do
  x <- chooseInt (-100, largeurZoneJeu + 100)
  y <- chooseInt (-100, hauteurZoneJeu + 100)
  w <- chooseInt (1, 80)
  h <- chooseInt (1, 80)
  pure (Rectangle x y w h)

genMurGaucheFini :: Gen Hitbox
genMurGaucheFini = do
  x0 <- chooseInt (0, 80)
  y0 <- chooseInt (-100, 50)
  dy <- chooseInt (1, 80)
  n <- chooseInt (2, 10)
  pure (MurGauche [(x0 + (i `mod` 2) * 15, y0 + i * dy) | i <- [0 .. n - 1]])

genMurDroitFini :: Gen Hitbox
genMurDroitFini = do
  x0 <- chooseInt (largeurZoneJeu - 80, largeurZoneJeu)
  y0 <- chooseInt (-100, 50)
  dy <- chooseInt (1, 80)
  n <- chooseInt (2, 10)
  pure (MurDroit [(x0 - (i `mod` 2) * 15, y0 + i * dy) | i <- [0 .. n - 1]])

genMobileHitbox :: Gen Hitbox
genMobileHitbox = sized go
  where
    go n
      | n <= 0 = oneof [genPointHitbox, genDisqueHitbox, genRectHitbox]
      | otherwise = frequency
          [ (5, genPointHitbox)
          , (5, genDisqueHitbox)
          , (7, genRectHitbox)
          , (2, do
                k <- chooseInt (2, 4)
                hs <- vectorOf k (go (n `div` 2))
                pure (Composee hs))
          ]

genHitbox :: Int -> Gen Hitbox
genHitbox n
  | n <= 0 = oneof [genPointHitbox, genDisqueHitbox, genRectHitbox]
  | otherwise = frequency
      [ (5, genPointHitbox)
      , (5, genDisqueHitbox)
      , (7, genRectHitbox)
      , (2, do
            k <- chooseInt (2, 4)
            hs <- vectorOf k (genHitbox (n `div` 2))
            pure (Composee hs))
      , (1, genMurGaucheFini)
      , (1, genMurDroitFini)
      ]

instance Arbitrary Hitbox where
  arbitrary = sized genHitbox

instance Arbitrary Cadence where
  arbitrary = genCadence

genCadence :: Gen Cadence
genCadence = do
  a <- chooseInt (1, 20)
  r <- chooseInt (0, a - 1)
  pure (Cadence a r)

genCadencePrete :: Gen Cadence
genCadencePrete = Cadence <$> chooseInt (1, 20) <*> pure 0

genCadenceEnAttente :: Gen Cadence
genCadenceEnAttente = do
  a <- chooseInt (2, 20)
  r <- chooseInt (1, a - 1)
  pure (Cadence a r)

instance Arbitrary PartiesVaisseau where
  arbitrary = genPartiesVaisseau

genPartiesVaisseau :: Gen PartiesVaisseau
genPartiesVaisseau = do
  x <- chooseInt (50, largeurZoneJeu - 100)
  y <- chooseInt (20, hauteurZoneJeu - 100)
  pure $ unsafeRight
    "genPartiesVaisseau: mkPartiesVaisseauStandard impossible"
    (mkPartiesVaisseauStandard x y)

instance Arbitrary Projectile where
  arbitrary = genProjectile

genProjectile :: Gen Projectile
genProjectile = do
  h <- genMobileHitbox
  d <- arbitrary
  c <- genCadence
  owner <- arbitrary
  joueur <- case owner of
    TirJoueuse -> frequency
      [ (1, pure Nothing)
      , (4, Just <$> chooseInt (0, 3))
      ]
    TirEnnemi -> pure Nothing
  pure (Projectile h d c owner joueur)

genProjectileJoueuse :: Gen Projectile
genProjectileJoueuse = do
  h <- genMobileHitbox
  d <- arbitrary
  c <- genCadence
  i <- chooseInt (0, 3)
  pure (Projectile h d c TirJoueuse (Just i))

genProjectileEnnemi :: Gen Projectile
genProjectileEnnemi = do
  h <- genMobileHitbox
  d <- arbitrary
  c <- genCadence
  pure (Projectile h d c TirEnnemi Nothing)

instance Arbitrary Obstacle where
  arbitrary = genObstacleSimple

genObstacleSimple :: Gen Obstacle
genObstacleSimple = Obstacle <$> genRectHitbox

instance Arbitrary VaisseauJoueuse where
  arbitrary = genVaisseau

genVaisseau :: Gen VaisseauJoueuse
genVaisseau = do
  forme <- genPartiesVaisseau
  pv <- chooseInt (0, pvMaxJoueuse)
  essais <- chooseInt (0, 5)
  cad <- genCadence
  pure (VaisseauJoueuse forme pv essais cad)

genVaisseauActif :: Gen VaisseauJoueuse
genVaisseauActif = do
  forme <- genPartiesVaisseau
  pv <- chooseInt (1, pvMaxJoueuse)
  essais <- chooseInt (1, 5)
  cad <- genCadence
  pure (VaisseauJoueuse forme pv essais cad)

genVaisseauElimine :: Gen VaisseauJoueuse
genVaisseauElimine = do
  forme <- genPartiesVaisseau
  cad <- genCadence
  oneof
    [ pure (VaisseauJoueuse forme 0 0 cad)
    , do pv <- chooseInt (0, pvMaxJoueuse)
         pure (VaisseauJoueuse forme pv 0 cad)
    ]

instance Arbitrary Oracle where
  arbitrary = genOracle

genOracle :: Gen Oracle
genOracle = frequency
  [ (4, do
        k <- chooseInt (1, 8)
        actions <- vectorOf k arbitrary
        i <- chooseInt (0, 20)
        pure (Scripted actions i))
  , (1, pure FromInt)
  ]

instance Arbitrary PV where
  arbitrary = PV <$> chooseInt (1, 20)

instance Arbitrary Ennemi where
  arbitrary = genEnnemiSimple

genEnnemi :: Gen Ennemi
genEnnemi = genEnnemiSimple

genEnnemiSimple :: Gen Ennemi
genEnnemiSimple = do
  h <- genRectHitbox
  pv <- chooseInt (1, 8)
  cad <- genCadence
  o <- genOracle
  pure (Ennemi h (PV pv) o cad)

instance Arbitrary Meteore where
  arbitrary = genMeteoreSimple

genMeteoreSimple :: Gen Meteore
genMeteoreSimple = do
  x <- chooseInt (50, largeurZoneJeu - 50)
  y <- chooseInt (-50, hauteurZoneJeu + 100)
  r <- chooseInt (1, 30)
  cad <- genCadence
  degats <- chooseInt (1, 5)
  pure (Meteore (Disque x y r) cad degats)

instance Arbitrary Score where
  arbitrary = Score <$> chooseInt (0, 100000)

genScores :: Gen [Score]
genScores = listOf arbitrary

instance Arbitrary Evenement where
  arbitrary = oneof
    [ AppEnnemi <$> genEnnemiSimple
    , AppObstacle <$> genObstacleSimple
    , AppProjectile <$> genProjectile
    , AppMeteore <$> genMeteoreSimple
    , DisparEnnemi <$> chooseInt (-2, 8)
    , DisparObstacle <$> chooseInt (-2, 8)
    , DisparMeteore <$> chooseInt (-2, 8)
    ]

instance Arbitrary EvenementPlanifie where
  arbitrary = genEvenementPlanifieSimple

genEvenementPlanifieSimple :: Gen EvenementPlanifie
genEvenementPlanifieSimple = do
  tour <- chooseInt (0, 30)
  evt <- arbitrary
  pure (EvenementPlanifie tour evt)

mursTest :: MursNiveau
mursTest = unsafeRight "mursTest invalide" $
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

genMoteur :: Gen Moteur
genMoteur = do
  nbObs <- chooseInt (0, 4)
  nbProj <- chooseInt (0, 5)
  nbEnn <- chooseInt (0, 4)
  nbMet <- chooseInt (0, 2)
  nbJ <- chooseInt (1, 2)

  obs <- vectorOf nbObs genObstacleSimple
  projs <- vectorOf nbProj genProjectile
  enns <- vectorOf nbEnn genEnnemiSimple
  meteores <- vectorOf nbMet genMeteoreSimple
  jous <- vectorOf nbJ genVaisseauActif

  cad <- genCadence
  evts <- listOf genEvenementPlanifieSimple
  tour <- chooseInt (0, 30)
  seed <- arbitrary

  case mkMoteurTest obs projs enns jous cad evts tour seed of
    Right m -> pure (m { mMeteores = meteores })
    Left _  -> genMoteur

instance Arbitrary Moteur where
  arbitrary = genMoteur

moteurMinimal :: Moteur
moteurMinimal =
  let cad = unsafeRight "cadence minimale invalide" (mkCadence 1)
      forme = unsafeRight "forme minimale invalide" (mkPartiesVaisseauStandard 100 100)
      v = unsafeRight "vaisseau minimal invalide" (mkVaisseauJoueuse forme 3 2 cad)
  in unsafeRight "moteur minimal invalide" (mkMoteurTest [] [] [] [v] cad [] 0 0)
