{-# LANGUAGE OverloadedStrings #-}

module Main where

import SpecHelpers
import qualified EngineSpec
import qualified HitboxSpec
import qualified VaisseauSpec

import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck

import Model.Hitbox
import Model.Objects

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

  describe "Tests des collisions" $ do
    HitboxSpec.spec
    
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

  describe "Tests du Vaisseau" $ do
    VaisseauSpec.spec

  describe "Tests du Moteur" $ do
    EngineSpec.spec
