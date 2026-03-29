{-# OPTIONS_GHC -Wno-orphans #-}

module SpecHelpers where

import Lib
import Test.QuickCheck

-- Ça sert à contraindre les valeurs générées (ici: petites valeurs, pour éviter des trucs énormes).
-- comme Int va jusqu'à 2 millard
newtype SmallInt = SmallInt Int deriving (Show)
-- class Arbitrary a where
--      arbitrary :: Gen a
instance Arbitrary SmallInt where
  arbitrary = SmallInt <$> chooseInt (-100, 100) -- chooseInt (-100,100) :: Gen Int (génère un Int dans l’intervalle)
-- (<$>) c’est fmap:
-- (<$>) :: (a -> b) -> Gen a -> Gen b


isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft _        = False

isRightWith :: (b -> Bool) -> Either a b -> Bool
isRightWith pr (Right x) = pr x
isRightWith _  _         = False

{-
    Direction c’est un type avec plusieurs valeurs possibles:
    Haut, Bas, Gauche, Droite
    [minBound .. maxBound] = liste de toutes les directions
    elements = “choisis un élément au hasard dans la liste”
    Donc:
    QuickCheck choisit une direction au hasard.
-}
instance Arbitrary Direction where
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
