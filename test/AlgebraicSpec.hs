module AlgebraicSpec (spec) where

import Data.Monoid (Sum(..))
import Model.Engine
import Model.Objects
import Model.Score
import SpecHelpers
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck

scriptInt :: Gen (Script Int)
scriptInt = scriptFromList <$> listOf arbitrary

scriptIntSmall :: Gen (Script Int)
scriptIntSmall = scriptFromList <$> listOf (chooseInt (-20, 20))

scriptFunctions :: Script (Int -> Int)
scriptFunctions =
  scriptFromList [(+ 1), (* 2), subtract 3]

nonNegative :: Int -> Maybe Int
nonNegative n
  | n >= 0    = Just n
  | otherwise = Nothing

spec :: Spec
spec = do
  describe "Classes algébriques: Score" $ do
    prop "Score vérifie l'associativité de Semigroup" $
      \a b c -> prop_score_semigroup_assoc (a :: Score) b c

    prop "Score vérifie l'identité gauche de Monoid" $
      \s -> prop_score_monoid_left (s :: Score)

    prop "Score vérifie l'identité droite de Monoid" $
      \s -> prop_score_monoid_right (s :: Score)

    prop "scoreTotal est mconcat des scores" $
      forAll genScores $ \scores ->
        scoreTotal scores == mconcat scores

    prop "ajouterPoints correspond à l'action du monoïde des scores pour les points positifs" $
      forAll arbitrary $ \score ->
        forAll (chooseInt (1, 10000)) $ \points ->
          ajouterPoints points score == score <> Score points

  describe "Classes algébriques: PV" $ do
    prop "PV vérifie l'associativité de Semigroup" $
      \a b c -> prop_pv_semigroup_assoc (a :: PV) b c

    prop "la composition de deux PV valides conserve l'invariant" $
      \a b -> prop_pv_semigroup_preserve_inv (a :: PV) b

  describe "Classes algébriques: Script" $ do
    prop "Functor: identité" $
      forAll scriptInt $ \s ->
        prop_script_functor_id s

    prop "Functor: composition" $
      forAll scriptInt $ \s ->
        fmap ((+ 1) . (* 2)) s == (fmap (+ 1) . fmap (* 2)) s

    prop "Applicative: identité" $
      forAll scriptInt $ \s ->
        (pure id <*> s) == s

    prop "Applicative: homomorphisme" $
      \x ->
        ((pure (+ 1) <*> pure x) :: Script Int) == pure ((+ 1) (x :: Int))

    prop "Applicative: interchange" $
      \y ->
        (scriptFunctions <*> pure (y :: Int)) == (pure ($ y) <*> scriptFunctions)

    prop "Monad: identité gauche" $
      \x ->
        let f n = scriptFromList [n + 1, n * 2]
        in ((pure (x :: Int) >>= f) :: Script Int) == f x

    prop "Monad: identité droite" $
      forAll scriptInt $ \s ->
        (s >>= pure) == s

    prop "Monad: associativité" $
      forAll scriptIntSmall $ \s ->
        let f n = scriptFromList [n + 1, n * 2]
            g n = scriptFromList [n - 1, negate n]
        in ((s >>= f) >>= g) == (s >>= (\x -> f x >>= g))

    prop "Semigroup: associativité" $
      forAll scriptInt $ \a ->
        forAll scriptInt $ \b ->
          forAll scriptInt $ \c ->
            prop_script_semigroup_assoc a b c

    prop "Monoid: identité gauche" $
      forAll scriptInt $ \s ->
        prop_script_monoid_left s

    prop "Monoid: identité droite" $
      forAll scriptInt $ \s ->
        prop_script_monoid_right s

    prop "Foldable: foldMap accumule par le monoïde choisi" $
      forAll scriptInt $ \s ->
        foldMap Sum s == Sum (sum (scriptToList s))

    prop "Traversable: traverse inverse Script et Maybe" $
      forAll scriptIntSmall $ \s ->
        traverse nonNegative s == sequenceA (fmap nonNegative s)

    it "Semigroup/Monoid compose des fragments de script moteur" $ do
      let e1 = EvenementPlanifie 3 (DisparEnnemi 0)
          e2 = EvenementPlanifie 7 (DisparObstacle 1)
          s1 = scriptFromList [e1]
          s2 = scriptFromList [e2]
      scriptToList (s1 <> mempty <> s2) `shouldBe` [e1, e2]
