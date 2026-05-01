module EngineSpec (spec) where

import Model.Murs
import Model.Engine
import Model.Hitbox
import Model.Objects
import Model.Score
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

attendUneJoueuse :: [VaisseauJoueuse] -> (VaisseauJoueuse -> Expectation) -> Expectation
attendUneJoueuse joueuses verifier =
  case joueuses of
    [v] -> verifier v
    _   -> expectationFailure ("Une seule joueuse attendue, obtenu: " ++ show joueuses)

attendUnEnnemi :: [Ennemi] -> (Ennemi -> Expectation) -> Expectation
attendUnEnnemi ennemis verifier =
  case ennemis of
    [e] -> verifier e
    _   -> expectationFailure ("Un seul ennemi attendu, obtenu: " ++ show ennemis)

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

      attendUneJoueuse (mJoueuses m') $ \v' -> do
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

    it "Une collision directe joueuse/ennemi est gérée par les collisions spéciales" $ do
      let Right cad = mkCadence 1
          Right pv = mkPV 3
          v = vaisseauTest 10 10 3 2 cad
          e = Ennemi (Rectangle 10 18 24 28) pv (Scripted [Attendre] 0) cad
          Right m = mkMoteurTest [] [] [e] [v] cad [] 0 0
          m' = resoudreCollisionsEnnemis m

      map vjPv (mJoueuses m') `shouldBe` [2]

  describe "Moteur: score" $ do
    it "le score initial du moteur est nul" $ do
      let Right cad = mkCadence 1
          v = vaisseauTest 10 10 3 2 cad
          Right m = mkMoteurTest [] [] [] [v] cad [] 0 0

      scoreValeur (mScore m) `shouldBe` 0

    it "détruire un ennemi avec un tir joueur augmente le score" $ do
      let Right cad = mkCadence 1
          v = vaisseauTest 10 10 3 2 cad

          Right pv = mkPV 1
          Right hEnnemi = mkRectangle 200 300 30 34
          Right ennemi = mkEnnemi hEnnemi pv (Scripted [Attendre] 0) cad

          p = Projectile (Point 210 310) Haut cad TirJoueuse

          Right m = mkMoteurTest [] [p] [ennemi] [v] cad [] 0 0
          m' = resoudreCollisions m

      mEnnemis m' `shouldBe` []
      scoreValeur (mScore m') `shouldBe` scoreEnnemiDetruit 0

    it "le score gagné augmente avec le nombre de tours survécus" $ do
      scoreEnnemiDetruit 3000 `shouldSatisfy` (> scoreEnnemiDetruit 0)

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

  describe "Moteur: collisions spéciales des ennemis" $ do
    it "une collision ennemi/joueuse enlève 1 PV et pousse la joueuse vers le bas" $ do
      let Right cad = mkCadence 1
          v = vaisseauTest 100 100 3 2 cad

          Right pv = mkPV 2
          Right hEnnemi = mkRectangle 110 115 30 34
          Right ennemi = mkEnnemi hEnnemi pv (Scripted [Attendre] 0) cad

          Right m = mkMoteurTest [] [] [ennemi] [v] cad [] 0 0
          m' = resoudreCollisionsEnnemis m

      attendUneJoueuse (mJoueuses m') $ \v' -> do
        vjPv v' `shouldBe` 2
        snd (centreHitbox (vjHitbox v')) `shouldSatisfy`
          (< snd (centreHitbox (vjHitbox v)))

    it "un ennemi qui touche un obstacle est repoussé vers le haut" $ do
      let Right cad = mkCadence 1

          Right pv = mkPV 2
          Right hEnnemi = mkRectangle 200 300 30 34
          Right ennemi = mkEnnemi hEnnemi pv (Scripted [Attendre] 0) cad

          Right hObstacle = mkRectangle 195 295 50 50
          Right obstacle = mkObstacle hObstacle

          v = vaisseauTest 100 100 3 2 cad
          Right m = mkMoteurTest [obstacle] [] [ennemi] [v] cad [] 0 0
          m' = resoudreCollisionsEnnemis m

      attendUnEnnemi (mEnnemis m') $ \ennemi' ->
        snd (centreHitbox (eHitbox ennemi')) `shouldSatisfy`
          (> snd (centreHitbox (eHitbox ennemi)))

    it "un ennemi qui touche un météore disparaît" $ do
      let Right cad = mkCadence 1

          Right pv = mkPV 2
          Right hEnnemi = mkRectangle 300 400 30 34
          Right ennemi = mkEnnemi hEnnemi pv (Scripted [Attendre] 0) cad

          Right meteore = mkMeteoreRond 315 417 20 cad

          v = vaisseauTest 100 100 3 2 cad
          Right m = mkMoteurTest [] [] [ennemi] [v] cad [] 0 0

          m' = resoudreCollisionsEnnemis (m { mMeteores = [meteore] })

      mEnnemis m' `shouldBe` []
  
  describe "Moteur: configuration de partie" $ do
    it "la configuration par défaut crée une partie monojoueur" $ do
      let Right m = mkMoteurPartie configPartieDefaut

      length (mJoueuses m) `shouldBe` 1
      mObstacles m `shouldBe` []
      mProjectiles m `shouldBe` []
      mEnnemis m `shouldBe` []
      mMeteores m `shouldBe` []
      mScript m `shouldBe` []
      prop_inv_moteur m `shouldBe` True

    it "la configuration duo crée deux joueuses" $ do
      let Right m = mkMoteurPartie configPartieDuo

      length (mJoueuses m) `shouldBe` 2
      prop_inv_moteur m `shouldBe` True

    it "exempleMoteur reste compatible avec la configuration par défaut" $ do
      let Right m1 = exempleMoteur
          Right m2 = mkMoteurPartie configPartieDefaut

      length (mJoueuses m1) `shouldBe` length (mJoueuses m2)
      prop_inv_moteur m1 `shouldBe` True
      
  describe "Moteur: génération dynamique bornée" $ do
    it "préserve l'invariant du moteur" $ do
      let Right m = mkMoteurPartie configPartieDefaut
          m' = genererObjetsDynamiques m

      prop_inv_moteur m' `shouldBe` True

    it "ne dépasse jamais les quotas d'objets actifs" $ do
      let Right m0 = mkMoteurPartie configPartieDefaut
          mFinal = iterate finDeTourMoteur m0 !! 5000

      length (mEnnemis mFinal) `shouldSatisfy` (<= maxEnnemisDynamiques)
      length (mMeteores mFinal) `shouldSatisfy` (<= maxMeteoresDynamiques)
      length (mObstacles mFinal) `shouldSatisfy` (<= maxObstaclesDynamiques)
      prop_inv_moteur mFinal `shouldBe` True

    it "ajoute au moins un objet dynamique après assez de tours" $ do
      let Right m0 = mkMoteurPartie configPartieDefaut
          mFinal = iterate finDeTourMoteur m0 !! 600
          nbObjets =
              length (mEnnemis mFinal)
            + length (mMeteores mFinal)
            + length (mObstacles mFinal)

      nbObjets `shouldSatisfy` (> 0)