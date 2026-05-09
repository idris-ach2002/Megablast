module VaisseauSpec (spec) where

import Model.Hitbox
import Model.Objects
import Model.VaisseauForme
import SpecHelpers
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck

formeTest :: PartiesVaisseau
formeTest = unsafeRight "formeTest invalide" (mkPartiesVaisseauStandard 0 0)

spec :: Spec
spec = do
  describe "PartiesVaisseau: invariants et constructeurs" $ do
    it "mkPartiesVaisseau impose les formes attendues" $ do
      let corps = Rectangle 0 8 24 28
          rg = Disque 6 6 6
          rd = Disque 18 6 6
          cockpit = Just (Disque 12 28 4)
      mkPartiesVaisseau corps rg rd cockpit `shouldSatisfy` isRightWith prop_inv_partiesVaisseau
      mkPartiesVaisseau (Disque 0 0 3) rg rd cockpit `shouldSatisfy` isLeft
      mkPartiesVaisseau corps (Rectangle 0 0 1 1) rd cockpit `shouldSatisfy` isLeft
      mkPartiesVaisseau corps rg (Rectangle 0 0 1 1) cockpit `shouldSatisfy` isLeft

    prop "mkPartiesVaisseauStandard produit toujours une forme valide" $
      \(SmallInt x) (SmallInt y) ->
        isRightWith prop_inv_partiesVaisseau (mkPartiesVaisseauStandard x y)

    prop "translatePartiesVaisseau préserve l'invariant et respecte la post-condition" $
      forAll genPartiesVaisseau $ \pv ->
        \(SmallInt dx) (SmallInt dy) ->
          let pv' = translatePartiesVaisseau dx dy pv
          in prop_post_translatePartiesVaisseau pv pv'

    prop "hitboxPartiesVaisseau produit une hitbox composée valide" $
      forAll genPartiesVaisseau $ \pv ->
        let h = hitboxPartiesVaisseau pv
        in prop_post_hitboxPartiesVaisseau pv h

    it "pointHautCentre couvre tous les constructeurs de Hitbox" $ do
      pointHautCentre (Rectangle 10 20 4 6) `shouldBe` Point 12 26
      pointHautCentre (Disque 10 20 4) `shouldBe` Point 10 24
      pointHautCentre (Point 10 20) `shouldBe` Point 10 20
      pointHautCentre (Composee [Rectangle 1 2 3 4, Point 0 0]) `shouldBe` Point 2 6
      pointHautCentre (Composee []) `shouldBe` Point 0 0
      pointHautCentre (MurGauche [(7,8),(7,9)]) `shouldBe` Point 7 8
      pointHautCentre (MurGauche []) `shouldBe` Point 0 0
      pointHautCentre (MurDroit [(7,8),(7,9)]) `shouldBe` Point 7 8
      pointHautCentre (MurDroit []) `shouldBe` Point 0 0

  describe "VaisseauJoueuse: invariants et constructeurs" $ do
    it "mkVaisseauJoueuse rejette pv < 0, essais < 0 ou cadence invalide" $ do
      let cad = unsafeRight "VaisseauSpec: cad" (mkCadence 1)
      mkVaisseauJoueuse formeTest (-1) 2 cad `shouldSatisfy` isLeft
      mkVaisseauJoueuse formeTest 3 (-1) cad `shouldSatisfy` isLeft
      mkVaisseauJoueuse formeTest 3 2 (Cadence 0 0) `shouldSatisfy` isLeft

    prop "mkVaisseauJoueuse produit un vaisseau valide avec des entrées correctes" $
      forAll genPartiesVaisseau $ \forme ->
        forAll genCadence $ \cad ->
          \(NonNegative pv) (NonNegative essais) ->
            isRightWith prop_inv_vaisseau (mkVaisseauJoueuse forme pv essais cad)

    prop "vjHitbox et vjHitboxTir produisent des hitbox valides" $
      forAll genVaisseau $ \v ->
        prop_inv_hitbox (vjHitbox v) && prop_inv_hitbox (vjHitboxTir v)

  describe "VaisseauJoueuse: déplacements et cadence" $ do
    prop "deplaceVaisseau préserve l'invariant et respecte la post-condition" $
      forAll genVaisseau $ \v ->
        forAll arbitrary $ \dir ->
          prop_post_deplaceVaisseau dir v (deplaceVaisseau dir v)

    prop "repousseVaisseau annule exactement un déplacement dans la même direction" $
      forAll genVaisseau $ \v ->
        forAll arbitrary $ \dir ->
          prop_post_repousseVaisseau dir v (repousseVaisseau dir v)

    prop "essaieDeplacerVaisseau respecte sa post-condition" $
      forAll genVaisseau $ \v ->
        forAll arbitrary $ \dir ->
          prop_post_essaieDeplacerVaisseau dir v (essaieDeplacerVaisseau dir v)

    prop "si la cadence est prête, essaieDeplacerVaisseau déplace effectivement" $
      forAll genVaisseau $ \v0 ->
        forAll genCadencePrete $ \cad ->
          forAll arbitrary $ \dir ->
            let v = v0 { vjCadence = cad }
                v' = essaieDeplacerVaisseau dir v
            in vjForme v' == vjForme (deplaceVaisseau dir (v { vjCadence = snd (tickCadence cad) }))

    prop "si la cadence attend, essaieDeplacerVaisseau ne modifie que la cadence" $
      forAll genVaisseau $ \v0 ->
        forAll genCadenceEnAttente $ \cad ->
          forAll arbitrary $ \dir ->
            let v = v0 { vjCadence = cad }
                v' = essaieDeplacerVaisseau dir v
            in vjForme v' == vjForme v && vjCadence v' == snd (tickCadence cad)

  describe "VaisseauJoueuse: tir et dégâts" $ do
    prop "tirVaisseau génère un projectile valide vers le haut et signé par la joueuse" $
      forAll genVaisseau $ \v ->
        forAll genCadence $ \cadProj ->
          forAll (chooseInt (0, 4)) $ \i ->
            prop_pre_tirVaisseau i v cadProj
            && prop_post_tirVaisseau i v cadProj (tirVaisseau i v cadProj)

    it "tirVaisseau part de la hitbox de tir du vaisseau" $ do
      let cad = unsafeRight "VaisseauSpec: cad" (mkCadence 1)
          v = unsafeRight "VaisseauSpec: v" (mkVaisseauJoueuse formeTest 3 2 cad)
          p = tirVaisseau 0 v cad
      prHitbox p `shouldBe` vjHitboxTir v
      prJoueuse p `shouldBe` Just 0

    prop "subirDegat baisse les PV de 1 sans descendre sous 0" $
      forAll genVaisseau $ \v ->
        prop_post_subirDegat v (subirDegat v)

    it "subirDegat bloque les PV à 0" $ do
      let cad = unsafeRight "VaisseauSpec: cad" (mkCadence 1)
          v0 = unsafeRight "VaisseauSpec: v0" (mkVaisseauJoueuse formeTest 1 3 cad)
          v1 = subirDegat v0
          v2 = subirDegat v1
      vjPv v1 `shouldBe` 0
      vjPv v2 `shouldBe` 0
