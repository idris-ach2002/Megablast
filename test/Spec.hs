{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{- HLINT ignore "Use <$>" -}

module Main (main) where

import Lib

import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck

main :: IO ()
main = hspec $ do
  describe "Hitbox: invariants et constructeurs" $ do
    it "mkDisque rejette r<=0" $ do
      mkDisque 0 0 0 `shouldSatisfy` isLeft
      mkDisque 0 0 (-3) `shouldSatisfy` isLeft

    it "mkRectangle rejette w<=0 ou h<=0" $ do
      mkRectangle 0 0 0 1 `shouldSatisfy` isLeft
      mkRectangle 0 0 1 0 `shouldSatisfy` isLeft

    it "mkComposee rejette <2 composantes" $ do
      mkComposee [] `shouldSatisfy` isLeft
      mkComposee [Point 0 0] `shouldSatisfy` isLeft

    prop "Les smart constructors produisent une hitbox valide" $
      \(SmallInt x) (SmallInt y) (Positive r) (Positive w) (Positive h) ->
        isRightWith prop_inv_hitbox (mkDisque x y r)
        && isRightWith prop_inv_hitbox (mkRectangle x y w h)

  describe "Hitbox: collision et propriété Composee" $ do
    it "Exemple simple: composee de 2 points" $ do
      let p1 = Point 0 0
          p2 = Point 10 10
          h1 = Composee [p1,p2]
      collision h1 (Point 0 0) `shouldBe` True
      collision h1 (Point 10 10) `shouldBe` True
      collision h1 (Point 0 10) `shouldBe` False

    prop "Q1.4: si collision avec Composee[2 points], alors c'est un des 2 points" $
      \(SmallInt x1) (SmallInt y1) (SmallInt x2) (SmallInt y2) (SmallInt x) (SmallInt y) ->
        prop_composee2points_point (x1,y1) (x2,y2) (x,y)
    -- au lieu d’utiliser arbitrary, utilise ce générateur
  describe "Cadence" $ do
    prop "tickCadence préserve l'invariant" $
      forAll genCadence $ \c ->
        let (_, c') = tickCadence c in prop_inv_cadence c'

    prop "tickCadence respecte sa post-condition" $
      forAll genCadence $ \c ->
        prop_post_tickCadence c (tickCadence c)

    it "tickCadence fait bouger exactement tous les 'attente' tours" $ do
      let Right c0 = mkCadence 3
          moves = take 7 (map fst (iterate (tickCadence . snd) (tickCadence c0)))
      moves `shouldBe` [True,False,False,True,False,False,True]

  describe "Projectiles" $ do
    prop "finDeTourProjectile préserve l'invariant" $
      forAll genProjectile $ \p ->
        prop_inv_projectile (finDeTourProjectile p)

    prop "finDeTourProjectile respecte sa post-condition" $
      forAll genProjectile $ \p ->
        prop_post_finDeTourProjectile p (finDeTourProjectile p)

    it "Un projectile avec cadence=1 avance à chaque tour" $ do
      let Right c = mkCadence 1
          Right p0 = mkProjectile (Point 0 0) Haut c TirJoueuse
          p1 = finDeTourProjectile p0
          p2 = finDeTourProjectile p1
      prHitbox p1 `shouldBe` Point 0 1
      prHitbox p2 `shouldBe` Point 0 2

  describe "Ennemis" $ do
    it "Oracle Scripted est cyclique" $ do
      let o0 = Scripted [Attendre, Tirer] 0
          (a1,o1) = oracleStep o0 123
          (a2,o2) = oracleStep o1 999
          (a3,_)  = oracleStep o2 42
      [a1,a2,a3] `shouldBe` [Attendre, Tirer, Attendre]

    prop "finDeTourEnnemi préserve l'invariant (ennemi)" $
      forAll genEnnemi $ \e ->
        let (e', _mp) = finDeTourEnnemi 0 e
        in prop_inv_ennemi e'

--------------------------------------------------------------------------------
-- Helpers QuickCheck
--------------------------------------------------------------------------------

isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft _        = False

isRightWith :: (b -> Bool) -> Either a b -> Bool
isRightWith pr (Right x) = pr x
isRightWith _  _         = False

-- Ça sert à contraindre les valeurs générées (ici: petites valeurs, pour éviter des trucs énormes).
-- comme Int va jusqu'à 2 millard
newtype SmallInt = SmallInt Int deriving (Show)
-- class Arbitrary a where
--      arbitrary :: Gen a
instance Arbitrary SmallInt where
  arbitrary = SmallInt <$> chooseInt (-100, 100) -- chooseInt (-100,100) :: Gen Int (génère un Int dans l’intervalle)
-- (<$>) c’est fmap:
-- (<$>) :: (a -> b) -> Gen a -> Gen b


genCadence :: Gen Cadence
genCadence = do
  Positive a <- arbitrary
  r <- chooseInt (0, a - 1)
  pure (Cadence a r)

{-
    Direction c’est un type avec plusieurs valeurs possibles:
    Haut, Bas, Gauche, Droite
    [minBound .. maxBound] = liste de toutes les directions
    elements = “choisis un élément au hasard dans la liste”
    Donc:
    QuickCheck choisit une direction au hasard.
-}
instance Arbitrary Direction where
  arbitrary :: Gen Direction
  arbitrary = elements [minBound .. maxBound]

instance Arbitrary ProjectileOwner where
  arbitrary = elements [minBound .. maxBound]

{-
    Le n (une limite de profondeur)
    si on ne limites pas, QuickCheck peut créer:
    Composee [Composee [Composee [...]]] à l’infini.
    Donc n sert de frein.

    2 => On choisit:

    6 fois plus souvent une hitbox simple
    1 fois sur 7 une hitbox composée”
    frequency = “tirage au sort pondéré”.

    oneof = “choisis une option au hasard”.
    Ici les options: Point, Disque ,Rectangle

    composee

    k <- chooseInt (2,4) combien de sous-hitbox il y aura (2 à 4)
    2 minimum Parce que invariant: Composee doit contenir au moins 2 hitbox.
    hs <- vectorOf k (genHitbox (n div 2)) génère k hitbox, chacune avec une profondeur réduite

    n div 2
    Pour que ça s’arrête rapidement (la profondeur diminue).
-}
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

{-
    QuickCheck nous donne un paramètre de taille automatique (appelé "size").
    sized genHitbox lui dit: Utilise ce paramètre comme n pour limiter la profondeur.
-}
instance Arbitrary Hitbox where
  arbitrary = sized genHitbox

genProjectile :: Gen Projectile
genProjectile = do
  h <- arbitrary
  d <- arbitrary
  c <- genCadence
  o <- arbitrary
  pure (Projectile h d c o)

genEnnemi :: Gen Ennemi
genEnnemi = do
  h <- arbitrary
  Positive pv <- arbitrary
  cad <- genCadence
  let o = Scripted [Attendre, Tirer, Deplacer Gauche, Deplacer Droite] 0
  pure (Ennemi h (PV pv) o cad)