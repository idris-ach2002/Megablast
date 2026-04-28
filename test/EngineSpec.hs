module EngineSpec (spec) where

import Model.Murs
import Model.Engine
import Model.Hitbox
import Model.Objects
import Model.VaisseauForme
import SpecHelpers
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck

vaisseauTest :: Int -> Int -> Int -> Int -> Cadence -> VaisseauJoueuse
vaisseauTest x y pv essais cad =
  let forme =
        unsafeRight
          "vaisseauTest: forme invalide"
          (mkPartiesVaisseauStandard x y)
  in VaisseauJoueuse forme pv essais cad

pointDansVaisseauTest :: Hitbox
pointDansVaisseauTest =
  Point 22 30

spec :: Spec
spec = do
  describe "Moteur: invariants et constructeur" $ do
    prop "genMoteur produit un moteur valide" $
      forAll genMoteur prop_inv_moteur

    it "mkMoteur rejette un tour négatif" $ do
      let Right cad = mkCadence 1
          v = vaisseauTest 10 10 1 1 cad
      mkMoteurTest [] [] [] [v] cad [] (-1) 0 `shouldSatisfy` isLeft

    it "mkMoteur trie le script par tour croissant" $ do
      let Right cad = mkCadence 1
          v = vaisseauTest 10 10 1 1 cad
          e1 = EvenementPlanifie 10 (DisparObstacle 0)
          e2 = EvenementPlanifie 2  (DisparEnnemi 0)
          Right m = mkMoteurTest [] [] [] [v] cad [e1, e2] 0 0
      map epTour (mScript m) `shouldBe` [2, 10]

  describe "Moteur: propriétés de fin de tour" $ do
    prop "finDeTourMoteur incrémente le tour" $
      forAll genMoteur prop_tour_incremente

    prop "finDeTourMoteur préserve l'invariant" $
      forAll genMoteur prop_invariant_preserve

    prop "finDeTourMoteur n'augmente pas le nombre de joueuses" $
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
          v = vaisseauTest 10 10 3 2 cad
          p = Projectile pointDansVaisseauTest Bas cad TirEnnemi
          Right m = mkMoteurTest [] [p] [] [v] cad [] 0 0
          m' = resoudreCollisions m

      length (mProjectiles m') `shouldBe` 0
      map vjPv (mJoueuses m') `shouldBe` [2]

    it "Quand le dernier PV tombe à 0 et qu'il reste des essais, un essai est consommé" $ do
      let Right cad = mkCadence 1
          v = vaisseauTest 10 10 1 2 cad
          p = Projectile pointDansVaisseauTest Bas cad TirEnnemi
          Right m = mkMoteurTest [] [p] [] [v] cad [] 0 0
          m' = resoudreCollisions m
          [v'] = mJoueuses m'

      vjPv v' `shouldBe` pvRespawnMinimal
      vjEssais v' `shouldBe` 1
      mProjectiles m' `shouldBe` []

    it "Une joueuse avec 0 essai disparaît même s'il lui reste 1 PV" $ do
      let Right cad = mkCadence 1
          v = vaisseauTest 10 10 1 0 cad
          Right m = mkMoteurTest [] [] [] [v] cad [] 0 0
          m' = resoudreCollisions m

      mJoueuses m' `shouldBe` []

    it "Une joueuse qui consomme son dernier essai disparaît du moteur" $ do
      let Right cad = mkCadence 1
          v = vaisseauTest 10 10 1 1 cad
          p = Projectile pointDansVaisseauTest Bas cad TirEnnemi
          Right m = mkMoteurTest [] [p] [] [v] cad [] 0 0
          m' = resoudreCollisions m

      mJoueuses m' `shouldBe` []
      mProjectiles m' `shouldBe` []

    it "Une joueuse déjà morte PV=0 et essais=0 disparaît du moteur" $ do
      let Right cad = mkCadence 1
          v = vaisseauTest 10 10 0 0 cad
          Right m = mkMoteurTest [] [] [] [v] cad [] 0 0
          m' = resoudreCollisions m

      mJoueuses m' `shouldBe` []

    it "Une collision directe joueuse/ennemi enlève 1 PV à la joueuse" $ do
      let Right cad = mkCadence 1
          Right pv = mkPV 3
          v = vaisseauTest 10 10 3 2 cad
          e = Ennemi (Rectangle 10 18 24 28) pv (Scripted [Attendre] 0) cad
          Right m = mkMoteurTest [] [] [e] [v] cad [] 0 0
          m' = resoudreCollisions m

      map vjPv (mJoueuses m') `shouldBe` [2]

  describe "Moteur: collisions avec les murs" $ do
    it "Un contact avec le mur gauche repousse visiblement la joueuse" $ do
      let Right cad = mkCadence 1
          v = vaisseauTest 10 10 3 2 cad
          mur = MurGauche [(20, 0), (20, hauteurZoneJeu)]
          v' = joueuseToucheeMurGauche mur v

      v' `shouldNotBe` v
      collision (vjHitbox v') mur `shouldBe` False

    it "Un contact avec le mur droit repousse visiblement la joueuse" $ do
      let Right cad = mkCadence 1
          v = vaisseauTest 760 10 3 2 cad
          mur = MurDroit [(780, 0), (780, hauteurZoneJeu)]
          v' = joueuseToucheeMurDroit mur v

      v' `shouldNotBe` v
      collision (vjHitbox v') mur `shouldBe` False

  describe "Moteur: optimisation du scrolling" $ do
    it "mkMursNiveau transforme les murs infinis en murs finis" $ do
      let murG = mur_gauche_dents_scie 50 60
          murD = mur_droit_dents_scie largeurZoneJeu 50 60
          Right murs = mkMursNiveau murG murD

      murNombrePoints (mnMurGauche murs) `shouldSatisfy` (<= nombreMaxPointsMur)
      murNombrePoints (mnMurDroit murs)  `shouldSatisfy` (<= nombreMaxPointsMur)

    it "defileMurs garde un nombre borné de points après beaucoup de tours" $ do
      let murG = mur_gauche_dents_scie 50 60
          murD = mur_droit_dents_scie largeurZoneJeu 50 60
          Right murs0 = mkMursNiveau murG murD
          mursFinal = iterate defileMurs murs0 !! 5000

      prop_inv_mursNiveau mursFinal `shouldBe` True
      prop_post_defileMurs murs0 mursFinal `shouldBe` True
      murNombrePoints (mnMurGauche mursFinal) `shouldSatisfy` (<= nombreMaxPointsMur)
      murNombrePoints (mnMurDroit mursFinal)  `shouldSatisfy` (<= nombreMaxPointsMur)

    it "les obstacles sortis de l'écran sont supprimés en fin de tour" $ do
      let Right cad = mkCadence 1
          Right hObs = mkRectangle 30 (-200) 40 20
          Right obs = mkObstacle hObs
          v = vaisseauTest 10 10 3 2 cad
          Right m = mkMoteurTest [obs] [] [] [v] cad [] 0 0
          m' = finDeTourMoteur m

      mObstacles m' `shouldBe` []

  describe "Moteur: commandes sûres" $ do
    it "appliquerCommande ignore un indice de joueuse invalide" $ do
      let Right cad = mkCadence 1
          v = vaisseauTest 10 10 2 1 cad
          Right m = mkMoteurTest [] [] [] [v] cad [] 0 0
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

  describe "Moteur: apparition automatique des ennemis" $ do
    it "n'ajoute pas d'ennemi hors période d'apparition" $ do
      let Right cad = mkCadence 1
          v = vaisseauTest 10 10 3 2 cad
          Right m = mkMoteurTest [] [] [] [v] cad [] 1 0
          m' = genererEnnemiSiBesoin m

      length (mEnnemis m') `shouldBe` 0
      prop_inv_moteur m' `shouldBe` True

    it "ajoute un ennemi pendant une période d'apparition" $ do
      let Right cad = mkCadence 1
          v = vaisseauTest 10 10 3 2 cad
          Right m0 = mkMoteurTest [] [] [] [v] cad [] periodeApparitionEnnemi 0
          m' = genererEnnemiSiBesoin m0

      length (mEnnemis m') `shouldBe` 1
      prop_inv_moteur m' `shouldBe` True

    it "ne dépasse pas le nombre maximal d'ennemis actifs" $ do
      let Right cad = mkCadence 1
          v = vaisseauTest 10 10 3 2 cad
          Right pv = mkPV 2
          Right h = mkRectangle 200 700 30 34
          Right ennemi = mkEnnemi h pv (Scripted [Attendre] 0) cad
          ennemis = replicate maxEnnemisActifs ennemi
          Right m0 = mkMoteurTest [] [] ennemis [v] cad [] periodeApparitionEnnemi 0
          m' = genererEnnemiSiBesoin m0

      length (mEnnemis m') `shouldBe` maxEnnemisActifs
      prop_inv_moteur m' `shouldBe` True
  describe "Moteur: comportement intelligent des ennemis" $ do
    it "un ennemi à droite de la joueuse se déplace vers la gauche" $ do
      let Right cad = mkCadence 1
          v = vaisseauTest 100 100 3 2 cad
          Right pv = mkPV 2
          Right h = mkRectangle 300 700 30 34
          Right e = mkEnnemi h pv (Scripted [Attendre] 0) cad

          (e', _) =
            finDeTourEnnemiIntelligent 1 [v] e

      eHitbox e' `shouldBe` Rectangle 299 700 30 34

    it "un ennemi à gauche de la joueuse se déplace vers la droite" $ do
      let Right cad = mkCadence 1
          v = vaisseauTest 300 100 3 2 cad
          Right pv = mkPV 2
          Right h = mkRectangle 100 700 30 34
          Right e = mkEnnemi h pv (Scripted [Attendre] 0) cad

          (e', _) =
            finDeTourEnnemiIntelligent 1 [v] e

      eHitbox e' `shouldBe` Rectangle 101 700 30 34

    it "un ennemi produit un projectile ennemi quand sa cadence le permet" $ do
      let Right cad = mkCadence 1
          v = vaisseauTest 100 100 3 2 cad
          Right pv = mkPV 2
          Right h = mkRectangle 100 700 30 34
          Right e = mkEnnemi h pv (Scripted [Attendre] 0) cad

          (_, mp) =
            finDeTourEnnemiIntelligent 1 [v] e

      fmap prOwner mp `shouldBe` Just TirEnnemi
      fmap prDir mp `shouldBe` Just Bas

    prop "finDeTourEnnemiIntelligent préserve les invariants" $
      forAll genEnnemiSimple $ \e ->
        forAll genVaisseauActif $ \v ->
          let res = finDeTourEnnemiIntelligent 1 [v] e
          in prop_post_finDeTourEnnemiIntelligent 1 [v] e res