module VaisseauSpec (spec) where

import Lib
import SpecHelpers
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck

spec :: Spec
spec = do
  describe "VaisseauJoueuse: invariants et constructeurs" $ do

    it "mkVaisseauJoueuse rejette pv < 0 ou essais < 0" $ do
      let Right cad = mkCadence 1
          h = Point 0 0
      mkVaisseauJoueuse h (-1) 2 cad `shouldSatisfy` isLeft
      mkVaisseauJoueuse h 3 (-1) cad `shouldSatisfy` isLeft
    
    prop "mkVaisseauJoueuse produit un vaisseau valide avec des entrées correctes" $
      forAll genCadence $ \cad ->
        \(SmallInt x) (SmallInt y) ->
          let h = Point x y
          in isRightWith prop_inv_vaisseau (mkVaisseauJoueuse h 3 2 cad)

  describe "VaisseauJoueuse: déplacements et repoussements" $ do
    prop "deplaceVaisseau préserve l'invariant et respecte la post-condition" $
      forAll genVaisseau $ \v ->
        forAll arbitrary $ \dir ->
          prop_post_deplaceVaisseau dir v (deplaceVaisseau dir v)

    prop "repousseVaisseau annule exactement un déplacement dans la même direction" $
      forAll genVaisseau $ \v ->
        forAll arbitrary $ \dir ->
          prop_post_repousseVaisseau dir v (repousseVaisseau dir v)

  describe "VaisseauJoueuse: tir et dégâts" $ do
    prop "tirVaisseau génère un projectile valide vers le haut (TirJoueuse)" $
      forAll genVaisseau $ \v ->
        forAll genCadence $ \cadProj ->
          prop_post_tirVaisseau v cadProj (tirVaisseau v cadProj)

    prop "subirDegat baisse les PV de 1 sans descendre sous 0 et préserve l'invariant" $
      forAll genVaisseau $ \v ->
        prop_post_subirDegat v (subirDegat v)
        
    it "subirDegat bloque les PV à 0" $ do
      let Right cad = mkCadence 1
          Right v0 = mkVaisseauJoueuse (Point 0 0) 1 3 cad
          v1 = subirDegat v0 -- PV passe à 0
          v2 = subirDegat v1 -- PV reste à 0
      vjPv v1 `shouldBe` 0
      vjPv v2 `shouldBe` 0
