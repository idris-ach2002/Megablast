module EngineSpec (spec) where

import Engine
import Hitbox
import Objects
import SpecHelpers
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck

spec :: Spec
spec = do
  describe "Moteur: invariants et constructeur" $ do
    prop "genMoteur produit un moteur valide" $
      forAll genMoteur prop_inv_moteur

    it "mkMoteur rejette un tour négatif" $ do
      let Right cad = mkCadence 1
          Right v = mkVaisseauJoueuse (Rectangle 0 0 5 5) 1 1 cad
      mkMoteur [] [] [] [v] cad [] (-1) 0 `shouldSatisfy` isLeft

    it "mkMoteur trie le script par tour croissant" $ do
      let Right cad = mkCadence 1
          Right v = mkVaisseauJoueuse (Rectangle 0 0 5 5) 1 1 cad
          e1 = EvenementPlanifie 10 (DisparObstacle 0)
          e2 = EvenementPlanifie 2  (DisparEnnemi 0)
          Right m = mkMoteur [] [] [] [v] cad [e1, e2] 0 0
      map epTour (mScript m) `shouldBe` [2, 10]

  describe "Moteur: propriétés de fin de tour" $ do
    prop "finDeTourMoteur incrémente le tour" $
      forAll genMoteur prop_tour_incremente

    prop "finDeTourMoteur préserve l'invariant" $
      forAll genMoteur prop_invariant_preserve

    prop "finDeTourMoteur conserve le nombre de joueuses" $
      forAll genMoteur prop_joueuses_stables

    prop "finDeTourMoteur consomme les événements passés du script" $
      forAll genMoteur prop_script_diminue

    prop "finDeTourMoteur respecte sa post-condition" $
      forAll genMoteur $ \m ->
        prop_post_finDeTourMoteur m (finDeTourMoteur m)

    prop "finDeTourMoteurEither coïncide avec finDeTourMoteur sur un moteur valide" $
      forAll genMoteur $ \m ->
        finDeTourMoteurEither m == Right (finDeTourMoteur m)

  describe "Moteur: collisions et respawn" $ do
    it "Un projectile ennemi qui touche une joueuse disparaît et enlève 1 PV" $ do
      let Right cad = mkCadence 1
          v = VaisseauJoueuse (Rectangle 10 10 4 4) 3 2 cad
          p = Projectile (Point 10 10) Bas cad TirEnnemi
          Right m = mkMoteur [] [p] [] [v] cad [] 0 0
          m' = resoudreCollisions m
      length (mProjectiles m') `shouldBe` 0
      map vjPv (mJoueuses m') `shouldBe` [2]

    it "Quand le dernier PV tombe à 0, un essai est consommé et la joueuse réapparaît" $ do
      let Right cad = mkCadence 1
          v = VaisseauJoueuse (Rectangle 10 10 4 4) 1 2 cad
          p = Projectile (Point 10 10) Bas cad TirEnnemi
          Right m = mkMoteur [] [p] [] [v] cad [] 0 0
          m' = resoudreCollisions m
          [v'] = mJoueuses m'
      vjPv v' `shouldBe` pvRespawnMinimal
      vjEssais v' `shouldBe` 1

    it "Une collision directe joueuse/ennemi enlève 1 PV à la joueuse" $ do
      let Right cad = mkCadence 1
          Right pv = mkPV 3
          v = VaisseauJoueuse (Rectangle 10 10 4 4) 3 2 cad
          e = Ennemi (Rectangle 10 10 4 4) pv (Scripted [Attendre] 0) cad
          Right m = mkMoteur [] [] [e] [v] cad [] 0 0
          m' = resoudreCollisions m
      map vjPv (mJoueuses m') `shouldBe` [2]

  describe "Moteur: commandes sûres" $ do
    -- Ce test documente le nouveau contrat de sécurité : un indice invalide
    -- n'explose plus avec (!!), la commande est simplement ignorée.
    it "appliquerCommande ignore un indice de joueuse invalide" $ do
      let Right cad = mkCadence 1
          v = VaisseauJoueuse (Rectangle 0 0 4 4) 2 1 cad
          Right m = mkMoteur [] [] [] [v] cad [] 0 0
          (m', mp) = appliquerCommande 7 Tirer cad m
      m' `shouldBe` m
      mp `shouldBe` Nothing

    prop "appliquerCommande préserve l'invariant sur un indice valide" $
      forAll genMoteur $ \m ->
        forAll (chooseInt (0, length (mJoueuses m) - 1)) $ \i ->
          forAll arbitrary $ \dir ->
            let cad = mCadScroll m
                res = appliquerCommande i (Deplacer dir) cad m
            in prop_post_appliquerCommande i (Deplacer dir) cad m res
