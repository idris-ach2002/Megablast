module ObjectsSpec (spec) where

import Model.Hitbox
import Model.Objects
import SpecHelpers
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck

spec :: Spec
spec = do
  describe "Direction" $ do
    it "dirVector couvre toutes les directions du modèle" $ do
      dirVector Haut `shouldBe` (0, 1)
      dirVector Bas `shouldBe` (0, -1)
      dirVector Gauche `shouldBe` (-1, 0)
      dirVector Droite `shouldBe` (1, 0)

    prop "chaque direction déplace d'exactement un pixel sur un axe" $
      \d -> let (dx, dy) = dirVector d in abs dx + abs dy == 1

  describe "Cadence" $ do
    it "mkCadence rejette attente <= 0" $ do
      mkCadence 0 `shouldSatisfy` isLeft
      mkCadence (-2) `shouldSatisfy` isLeft

    prop "mkCadence construit toujours une cadence valide et prête" $
      \(Positive a) ->
        mkCadence a == Right (Cadence a 0)

    prop "tickCadence préserve l'invariant" $
      forAll genCadence $ \c ->
        let (_, c') = tickCadence c in prop_inv_cadence c'

    prop "tickCadence respecte sa post-condition" $
      forAll genCadence $ \c ->
        prop_post_tickCadence c (tickCadence c)

    prop "une cadence prête bouge maintenant puis remet restant à attente-1" $
      forAll genCadencePrete $ \c@(Cadence a _) ->
        tickCadence c == (True, Cadence a (a - 1))

    prop "une cadence en attente ne bouge pas et décrémente restant" $
      forAll genCadenceEnAttente $ \c@(Cadence a r) ->
        tickCadence c == (False, Cadence a (r - 1))

    it "tickCadence fait bouger exactement tous les 'attente' tours" $ do
      let c0 = unsafeRight "ObjectsSpec: c0" (mkCadence 3)
          moves = take 7 (map fst (iterate (tickCadence . snd) (tickCadence c0)))
      moves `shouldBe` [True, False, False, True, False, False, True]

  describe "Obstacle" $ do
    prop "mkObstacle accepte les hitbox valides" $
      forAll genMobileHitbox $ \h ->
        isRightWith prop_inv_obstacle (mkObstacle h)

    it "mkObstacle rejette une hitbox invalide" $ do
      mkObstacle (Rectangle 0 0 0 2) `shouldSatisfy` isLeft
      mkObstacle (Disque 0 0 0) `shouldSatisfy` isLeft

    prop "defileObstacle préserve l'invariant" $
      forAll genObstacleSimple $ \o ->
        prop_inv_obstacle (defileObstacle o)

    it "defileObstacle descend exactement d'un pixel" $ do
      defileObstacle (Obstacle (Rectangle 10 20 3 4))
        `shouldBe` Obstacle (Rectangle 10 19 3 4)

    prop "finDeTourObstacles respecte sa pré/post-condition" $
      forAll genCadence $ \cad ->
        forAll (listOf genObstacleSimple) $ \obs ->
          prop_pre_finDeTourObstacles cad obs
          && prop_post_finDeTourObstacles cad obs (finDeTourObstacles cad obs)

  describe "Projectile" $ do
    prop "mkProjectile produit un projectile valide" $
      forAll genMobileHitbox $ \h ->
        forAll arbitrary $ \d ->
          forAll genCadence $ \cad ->
            forAll arbitrary $ \owner ->
              isRightWith prop_inv_projectile (mkProjectile h d cad owner)

    it "un projectile ennemi ne peut pas référencer une joueuse" $ do
      let cad = unsafeRight "ObjectsSpec: cad" (mkCadence 1)
      prop_inv_projectile (Projectile (Point 0 0) Bas cad TirEnnemi Nothing) `shouldBe` True
      prop_inv_projectile (Projectile (Point 0 0) Bas cad TirEnnemi (Just 0)) `shouldBe` False

    prop "avanceProjectile respecte sa post-condition" $
      forAll genProjectile $ \p ->
        prop_post_avanceProjectile p (avanceProjectile p)

    prop "avanceProjectile translate la hitbox selon dirVector" $
      forAll genProjectile $ \p ->
        let (dx, dy) = dirVector (prDir p)
        in prHitbox (avanceProjectile p) == translateHitbox dx dy (prHitbox p)

    prop "finDeTourProjectile préserve l'invariant et respecte la post-condition" $
      forAll genProjectile $ \p ->
        let p' = finDeTourProjectile p
        in prop_inv_projectile p' && prop_post_finDeTourProjectile p p'

    it "un projectile avec cadence=1 avance à chaque tour" $ do
      let c = unsafeRight "ObjectsSpec: c" (mkCadence 1)
          p0 = unsafeRight "ObjectsSpec: p0" (mkProjectile (Point 0 0) Haut c TirJoueuse)
          p1 = finDeTourProjectile p0
          p2 = finDeTourProjectile p1
      prHitbox p1 `shouldBe` Point 0 1
      prHitbox p2 `shouldBe` Point 0 2

    it "indiceJoueuseProjectile est défini uniquement pour les tirs de joueuse" $ do
      let cad = unsafeRight "ObjectsSpec: cad" (mkCadence 1)
          pJ = Projectile (Point 0 0) Haut cad TirJoueuse (Just 1)
          pE = Projectile (Point 0 0) Bas cad TirEnnemi Nothing
      indiceJoueuseProjectile pJ `shouldBe` Just 1
      indiceJoueuseProjectile pE `shouldBe` Nothing

  describe "Oracle et Ennemi" $ do
    it "actionFromInt couvre les cinq branches prévues" $ do
      map actionFromInt [0..4] `shouldBe`
        [Attendre, Tirer, Deplacer Gauche, Deplacer Droite, Deplacer Bas]

    it "oracleStep sur Scripted est cyclique et incrémente l'indice" $ do
      let o0 = Scripted [Attendre, Tirer] 0
          (a1, o1) = oracleStep o0 123
          (a2, o2) = oracleStep o1 999
          (a3, o3) = oracleStep o2 42
      [a1, a2, a3] `shouldBe` [Attendre, Tirer, Attendre]
      o3 `shouldBe` Scripted [Attendre, Tirer] 3

    prop "oracleStep respecte sa post-condition" $
      forAll genOracle $ \oracle ->
        \n -> prop_post_oracleStep oracle n (oracleStep oracle n)

    it "mkPV rejette les valeurs non strictement positives" $ do
      mkPV 0 `shouldSatisfy` isLeft
      mkPV (-1) `shouldSatisfy` isLeft
      mkPV 1 `shouldSatisfy` isRightWith prop_inv_pv

    prop "mkEnnemi produit un ennemi valide avec entrées valides" $
      forAll genRectHitbox $ \h ->
        forAll arbitrary $ \pv ->
          forAll genOracle $ \oracle ->
            forAll genCadence $ \cad ->
              isRightWith prop_inv_ennemi (mkEnnemi h pv oracle cad)

    prop "finDeTourEnnemi préserve l'invariant et respecte la post-condition" $
      forAll genEnnemiSimple $ \e ->
        \rnd -> prop_post_finDeTourEnnemi rnd e (finDeTourEnnemi rnd e)

    it "un ennemi Scripted Deplacer modifie sa hitbox d'un pixel" $ do
      let cad = unsafeRight "ObjectsSpec: cad" (mkCadence 10)
          pv = unsafeRight "ObjectsSpec: pv" (mkPV 2)
          e = unsafeRight "ObjectsSpec: e" (mkEnnemi (Rectangle 10 10 3 4) pv (Scripted [Deplacer Droite] 0) cad)
          (e', mp) = finDeTourEnnemi 0 e
      eHitbox e' `shouldBe` Rectangle 11 10 3 4
      mp `shouldBe` Nothing

    it "un ennemi Scripted Tirer crée un projectile ennemi si sa cadence est prête" $ do
      let cad = unsafeRight "ObjectsSpec: cad" (mkCadence 1)
          pv = unsafeRight "ObjectsSpec: pv" (mkPV 2)
          e = unsafeRight "ObjectsSpec: e" (mkEnnemi (Rectangle 10 10 3 4) pv (Scripted [Tirer] 0) cad)
          (_, mp) = finDeTourEnnemi 0 e
      fmap prOwner mp `shouldBe` Just TirEnnemi
      fmap prDir mp `shouldBe` Just Bas
      fmap prJoueuse mp `shouldBe` Just Nothing
