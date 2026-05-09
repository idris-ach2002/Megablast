module EngineSpec (spec) where

import Model.Engine
import Model.Hitbox
import Model.Murs
import Model.Objects
import Model.VaisseauForme
import SpecHelpers
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck

vaisseauTest :: Int -> Int -> Int -> Int -> Cadence -> VaisseauJoueuse
vaisseauTest x y pv essais cad =
  let forme = unsafeRight "vaisseauTest: forme invalide" (mkPartiesVaisseauStandard x y)
  in VaisseauJoueuse forme pv essais cad

pointDansVaisseauTest :: Hitbox
pointDansVaisseauTest = Point 22 30

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
  describe "Engine.Types: MursNiveau, EvenementPlanifie et Moteur" $ do
    prop "genMoteur produit un moteur valide" $
      forAll genMoteur prop_inv_moteur

    it "mkMursNiveau exige un mur gauche et un mur droit valides" $ do
      mkMursNiveau (Point 0 0) (MurDroit [(10,0),(10,10)]) `shouldSatisfy` isLeft
      mkMursNiveau (MurGauche [(0,0),(0,10)]) (Point 0 0) `shouldSatisfy` isLeft
      mkMursNiveau (MurGauche [(0,0),(0,10)]) (MurDroit [(10,0),(10,10)])
        `shouldSatisfy` isRightWith prop_inv_mursNiveau

    it "mkMoteur rejette chaque famille invalide du modèle" $ do
      let cad = unsafeRight "EngineSpec: cad" (mkCadence 1)
          v = vaisseauTest 10 10 1 1 cad
          pv = unsafeRight "EngineSpec: pv" (mkPV 1)
          ennemi = unsafeRight "EngineSpec: ennemi" (mkEnnemi (Rectangle 100 100 20 20) pv (Scripted [Attendre] 0) cad)
          obstacle = unsafeRight "EngineSpec: obstacle" (mkObstacle (Rectangle 10 10 10 10))
          projectile = unsafeRight "EngineSpec: projectile" (mkProjectile (Point 0 0) Haut cad TirJoueuse)
      mkMoteurTest [Obstacle (Rectangle 0 0 0 2)] [] [] [v] cad [] 0 0 `shouldSatisfy` isLeft
      mkMoteurTest [obstacle] [Projectile (Disque 0 0 0) Haut cad TirJoueuse Nothing] [] [v] cad [] 0 0 `shouldSatisfy` isLeft
      mkMoteurTest [obstacle] [projectile] [ennemi { ePV = PV 0 }] [v] cad [] 0 0 `shouldSatisfy` isLeft
      mkMoteurTest [obstacle] [projectile] [ennemi] [v { vjPv = -1 }] cad [] 0 0 `shouldSatisfy` isLeft
      mkMoteurTest [obstacle] [projectile] [ennemi] [v] (Cadence 0 0) [] 0 0 `shouldSatisfy` isLeft
      mkMoteurTest [obstacle] [projectile] [ennemi] [v] cad [] (-1) 0 `shouldSatisfy` isLeft

    it "mkMoteur trie le script par tour croissant et initialise les scores" $ do
      let cad = unsafeRight "EngineSpec: cad" (mkCadence 1)
          v = vaisseauTest 10 10 1 1 cad
          e1 = EvenementPlanifie 10 (DisparObstacle 0)
          e2 = EvenementPlanifie 2  (DisparEnnemi 0)
          m = unsafeRight "EngineSpec: m" (mkMoteurTest [] [] [] [v] cad [e1, e2] 0 0)
      map epTour (mScript m) `shouldBe` [2, 10]
      mScores m `shouldBe` [scoreNul]
      mScore m `shouldBe` scoreNul

  describe "Engine.Murs: fenêtrage et défilement des murs" $ do
    it "mkMursNiveau transforme les murs infinis en murs finis" $ do
      let murG = mur_gauche_dents_scie 50 60
          murD = mur_droit_dents_scie largeurZoneJeu 50 60
          murs = unsafeRight "EngineSpec: murs" (mkMursNiveau murG murD)
      murNombrePoints (mnMurGauche murs) `shouldSatisfy` (<= nombreMaxPointsMur)
      murNombrePoints (mnMurDroit murs)  `shouldSatisfy` (<= nombreMaxPointsMur)
      prop_inv_mursNiveau murs `shouldBe` True

    prop "defileMurs respecte sa post-condition" $
      let murs = unsafeRight "EngineSpec: murs" (mkMursNiveau (mur_gauche_dents_scie 50 60) (mur_droit_dents_scie largeurZoneJeu 50 60))
      in prop_post_defileMurs murs (defileMurs murs)

    it "defileMurs garde un nombre borné de points après beaucoup de tours" $ do
      let murs0 = unsafeRight "EngineSpec: murs0" (mkMursNiveau (mur_gauche_dents_scie 50 60) (mur_droit_dents_scie largeurZoneJeu 50 60))
          mursFinal = iterate defileMurs murs0 !! 500
      prop_inv_mursNiveau mursFinal `shouldBe` True
      murNombrePoints (mnMurGauche mursFinal) `shouldSatisfy` (<= nombreMaxPointsMur)
      murNombrePoints (mnMurDroit mursFinal)  `shouldSatisfy` (<= nombreMaxPointsMur)

  describe "Engine.Step: événements et fin de tour" $ do
    it "appliquerEvenement couvre tous les constructeurs Evenement" $ do
      let cad = unsafeRight "EngineSpec: cad" (mkCadence 1)
          pv = unsafeRight "EngineSpec: pv" (mkPV 1)
          ennemi = unsafeRight "EngineSpec: ennemi" (mkEnnemi (Rectangle 10 10 3 4) pv (Scripted [Attendre] 0) cad)
          obstacle = unsafeRight "EngineSpec: obstacle" (mkObstacle (Rectangle 20 20 3 4))
          projectile = unsafeRight "EngineSpec: projectile" (mkProjectile (Point 1 1) Haut cad TirJoueuse)
          meteore = unsafeRight "EngineSpec: meteore" (mkMeteoreRond 30 30 5 cad)
          m0 = moteurMinimal
          m1 = appliquerEvenement (AppEnnemi ennemi) m0
          m2 = appliquerEvenement (AppObstacle obstacle) m1
          m3 = appliquerEvenement (AppProjectile projectile) m2
          m4 = appliquerEvenement (AppMeteore meteore) m3
          m5 = appliquerEvenement (DisparEnnemi 0) m4
          m6 = appliquerEvenement (DisparObstacle 0) m5
          m7 = appliquerEvenement (DisparMeteore 0) m6
      length (mEnnemis m1) `shouldBe` 1
      length (mObstacles m2) `shouldBe` 1
      length (mProjectiles m3) `shouldBe` 1
      length (mMeteores m4) `shouldBe` 1
      mEnnemis m5 `shouldBe` []
      mObstacles m6 `shouldBe` []
      mMeteores m7 `shouldBe` []

    prop "finDeTourMoteur incrémente le tour" $
      forAll genMoteur prop_tour_incremente

    prop "finDeTourMoteur préserve l'invariant" $
      forAll genMoteur prop_invariant_preserve

    prop "les slots joueuses restent stables" $
      forAll genMoteur prop_joueuses_stables

    prop "finDeTourMoteur consomme les événements passés du script" $
      forAll genMoteur prop_script_diminue

    prop "finDeTourMoteur respecte sa post-condition" $
      forAll genMoteur $ \m ->
        prop_post_finDeTourMoteur m (finDeTourMoteur m)

    prop "finDeTourMoteurEither coïncide avec finDeTourMoteur sur un moteur valide" $
      forAll genMoteur $ \m ->
        finDeTourMoteurEither m == Right (finDeTourMoteur m)

    it "finDeTourMoteurEither rejette un moteur invalide" $ do
      let m = moteurMinimal { mTour = -1 }
      finDeTourMoteurEither m `shouldSatisfy` isLeft

    it "les obstacles sortis de l'écran sont supprimés en fin de tour" $ do
      let cad = unsafeRight "EngineSpec: cad" (mkCadence 1)
          hObs = unsafeRight "EngineSpec: hObs" (mkRectangle 30 (-200) 40 20)
          obs = unsafeRight "EngineSpec: obs" (mkObstacle hObs)
          v = vaisseauTest 10 10 3 2 cad
          m = unsafeRight "EngineSpec: m" (mkMoteurTest [obs] [] [] [v] cad [] 0 0)
          m' = finDeTourMoteur m
      mObstacles m' `shouldBe` []

    it "horsEcranHitbox couvre tous les constructeurs de Hitbox" $ do
      horsEcranHitbox 100 100 10 (Point (-11) 0) `shouldBe` True
      horsEcranHitbox 100 100 10 (Disque 50 50 5) `shouldBe` False
      horsEcranHitbox 100 100 10 (Rectangle 150 0 5 5) `shouldBe` True
      horsEcranHitbox 100 100 10 (Composee [Point (-20) 0, Point 200 0]) `shouldBe` True
      horsEcranHitbox 100 100 10 (MurGauche [(0,0),(0,10)]) `shouldBe` False
      horsEcranHitbox 100 100 10 (MurDroit [(100,0),(100,10)]) `shouldBe` False

  describe "Engine.Collisions: joueuses, projectiles, ennemis et score" $ do
    prop "supprimerJoueusesEliminees conserve les slots joueurs" $
      forAll (listOf genVaisseau) $ \js ->
        let js' = supprimerJoueusesEliminees js
        in prop_post_supprimerJoueusesEliminees js js'
           && js' == js

    prop "joueusesActives est exactement le filtre des joueuses encore en jeu" $
      forAll (listOf genVaisseau) $ \js ->
        joueusesActives js == filter joueuseEncoreEnJeu js

    it "reapparaitreJoueuse consomme un essai et restaure les PV si possible" $ do
      let cad = unsafeRight "EngineSpec: cad" (mkCadence 1)
          v = vaisseauTest 10 10 0 2 cad
          v' = reapparaitreJoueuse v
      vjPv v' `shouldBe` pvRespawnJoueuse
      vjEssais v' `shouldBe` 1

    it "reapparaitreJoueuse élimine la joueuse au dernier essai" $ do
      let cad = unsafeRight "EngineSpec: cad" (mkCadence 1)
          v = vaisseauTest 10 10 0 1 cad
          v' = reapparaitreJoueuse v
      vjPv v' `shouldBe` 0
      vjEssais v' `shouldBe` 0

    prop "reapparaitreJoueuse respecte sa post-condition" $
      forAll genVaisseau prop_post_reapparaitreJoueuse

    it "un projectile ennemi qui touche une joueuse disparaît et enlève 1 PV" $ do
      let cad = unsafeRight "EngineSpec: cad" (mkCadence 1)
          v = vaisseauTest 10 10 3 2 cad
          p = Projectile pointDansVaisseauTest Bas cad TirEnnemi Nothing
          m = unsafeRight "EngineSpec: m" (mkMoteurTest [] [p] [] [v] cad [] 0 0)
          m' = resoudreCollisions m
      length (mProjectiles m') `shouldBe` 0
      map vjPv (mJoueuses m') `shouldBe` [2]

    it "une joueuse qui consomme son dernier essai reste dans son slot mais devient inactive" $ do
      let cad = unsafeRight "EngineSpec: cad" (mkCadence 1)
          v = vaisseauTest 10 10 1 1 cad
          p = Projectile pointDansVaisseauTest Bas cad TirEnnemi Nothing
          m = unsafeRight "EngineSpec: m" (mkMoteurTest [] [p] [] [v] cad [] 0 0)
          m' = resoudreCollisions m
      length (mJoueuses m') `shouldBe` 1
      mProjectiles m' `shouldBe` []
      attendUneJoueuse (mJoueuses m') $ \v' -> do
        vjPv v' `shouldBe` 0
        vjEssais v' `shouldBe` 0
        joueuseEncoreEnJeu v' `shouldBe` False

    it "une joueuse déjà éliminée n'est pas supprimée physiquement du moteur" $ do
      let cad = unsafeRight "EngineSpec: cad" (mkCadence 1)
          v = vaisseauTest 10 10 0 0 cad
          m = unsafeRight "EngineSpec: m" (mkMoteurTest [] [] [] [v] cad [] 0 0)
          m' = resoudreCollisions m
      mJoueuses m' `shouldBe` [v]
      joueusesActives (mJoueuses m') `shouldBe` []

    it "détruire un ennemi avec un tir joueur augmente le score de la bonne joueuse" $ do
      let cad = unsafeRight "EngineSpec: cad" (mkCadence 1)
          v = vaisseauTest 10 10 3 2 cad
          pv = unsafeRight "EngineSpec: pv" (mkPV 1)
          hEnnemi = unsafeRight "EngineSpec: hEnnemi" (mkRectangle 200 300 30 34)
          ennemi = unsafeRight "EngineSpec: ennemi" (mkEnnemi hEnnemi pv (Scripted [Attendre] 0) cad)
          p = Projectile (Point 210 310) Haut cad TirJoueuse (Just 0)
          m = unsafeRight "EngineSpec: m" (mkMoteurTest [] [p] [ennemi] [v] cad [] 0 0)
          m' = resoudreCollisions m
      mEnnemis m' `shouldBe` []
      mScores m' `shouldBe` [Score (scoreEnnemiDetruit 0)]
      mScore m' `shouldBe` scoreTotal (mScores m')

    prop "resoudreCollisions préserve l'invariant du moteur" $
      forAll genMoteur $ \m ->
        prop_inv_moteur (resoudreCollisions m)

    it "un contact avec le mur gauche repousse la joueuse hors du mur" $ do
      let cad = unsafeRight "EngineSpec: cad" (mkCadence 1)
          v = vaisseauTest 10 10 3 2 cad
          mur = MurGauche [(20, 0), (20, hauteurZoneJeu)]
          v' = joueuseToucheeMurGauche mur v
      v' `shouldNotBe` v
      collision (vjHitbox v') mur `shouldBe` False

    it "un contact avec le mur droit repousse la joueuse hors du mur" $ do
      let cad = unsafeRight "EngineSpec: cad" (mkCadence 1)
          v = vaisseauTest 760 10 3 2 cad
          mur = MurDroit [(780, 0), (780, hauteurZoneJeu)]
          v' = joueuseToucheeMurDroit mur v
      v' `shouldNotBe` v
      collision (vjHitbox v') mur `shouldBe` False

  describe "Engine.Commands: commandes sûres" $ do
    it "appliquerCommande ignore un indice de joueuse invalide" $ do
      let cad = unsafeRight "EngineSpec: cad" (mkCadence 1)
          v = vaisseauTest 10 10 2 1 cad
          m = unsafeRight "EngineSpec: m" (mkMoteurTest [] [] [] [v] cad [] 0 0)
          (m', mp) = appliquerCommande 7 Tirer cad m
      m' `shouldBe` m
      mp `shouldBe` Nothing

    it "appliquerCommande ignore une joueuse inactive" $ do
      let cad = unsafeRight "EngineSpec: cad" (mkCadence 1)
          v = vaisseauTest 10 10 0 0 cad
          m = unsafeRight "EngineSpec: m" (mkMoteurTest [] [] [] [v] cad [] 0 0)
          (m', mp) = appliquerCommande 0 Tirer cad m
      m' `shouldBe` m
      mp `shouldBe` Nothing

    prop "appliquerCommande respecte sa post-condition sur un indice valide" $
      forAll genMoteur $ \m ->
        forAll (chooseInt (0, length (mJoueuses m) - 1)) $ \i ->
          forAll arbitrary $ \action ->
            forAll genCadence $ \cad ->
              prop_post_appliquerCommande i action cad m (appliquerCommande i action cad m)

    it "Tirer ajoute un projectile signé par la joueuse" $ do
      let cad = unsafeRight "EngineSpec: cad" (mkCadence 1)
          v = vaisseauTest 10 10 2 1 cad
          m = unsafeRight "EngineSpec: m" (mkMoteurTest [] [] [] [v] cad [] 0 0)
          (m', mp) = appliquerCommande 0 Tirer cad m
      fmap prOwner mp `shouldBe` Just TirJoueuse
      fmap prJoueuse mp `shouldBe` Just (Just 0)
      length (mProjectiles m') `shouldBe` 1

  describe "Engine.EnnemiAI" $ do
    it "centreHitbox couvre tous les constructeurs de Hitbox" $ do
      centreHitbox (Point 1 2) `shouldBe` (1, 2)
      centreHitbox (Disque 1 2 3) `shouldBe` (1, 2)
      centreHitbox (Rectangle 0 0 10 20) `shouldBe` (5, 10)
      centreHitbox (Composee [Point 7 8, Point 0 0]) `shouldBe` (7, 8)
      centreHitbox (Composee []) `shouldBe` (0, 0)
      centreHitbox (MurGauche [(0,0),(0,10)]) `shouldBe` (0, 0)
      centreHitbox (MurDroit [(800,0),(800,10)]) `shouldBe` (largeurZoneJeu, 0)

    it "centreBasHitbox couvre tous les constructeurs de Hitbox" $ do
      centreBasHitbox (Point 1 2) `shouldBe` (1, 2)
      centreBasHitbox (Disque 1 2 3) `shouldBe` (1, -1)
      centreBasHitbox (Rectangle 0 0 10 20) `shouldBe` (5, 0)
      centreBasHitbox (Composee [Point 7 8, Point 0 0]) `shouldBe` (7, 8)
      centreBasHitbox (Composee []) `shouldBe` (0, 0)
      centreBasHitbox (MurGauche [(0,0),(0,10)]) `shouldBe` (0, 0)
      centreBasHitbox (MurDroit [(800,0),(800,10)]) `shouldBe` (largeurZoneJeu, 0)

    it "un ennemi à droite de la joueuse se déplace vers la gauche" $ do
      let cad = unsafeRight "EngineSpec: cad" (mkCadence 10)
          cadAttente = Cadence 10 5
          v = vaisseauTest 100 100 3 2 cad
          pv = unsafeRight "EngineSpec: pv" (mkPV 2)
          h = unsafeRight "EngineSpec: h" (mkRectangle 300 700 30 34)
          e = unsafeRight "EngineSpec: e" (mkEnnemi h pv (Scripted [Attendre] 0) cadAttente)
          (e', _) = finDeTourEnnemiIntelligent 1 [v] e
      eHitbox e' `shouldBe` Rectangle 299 700 30 34

    it "un ennemi à gauche de la joueuse se déplace vers la droite" $ do
      let cad = unsafeRight "EngineSpec: cad" (mkCadence 10)
          cadAttente = Cadence 10 5
          v = vaisseauTest 300 100 3 2 cad
          pv = unsafeRight "EngineSpec: pv" (mkPV 2)
          h = unsafeRight "EngineSpec: h" (mkRectangle 100 700 30 34)
          e = unsafeRight "EngineSpec: e" (mkEnnemi h pv (Scripted [Attendre] 0) cadAttente)
          (e', _) = finDeTourEnnemiIntelligent 1 [v] e
      eHitbox e' `shouldBe` Rectangle 101 700 30 34

    it "un ennemi produit un projectile ennemi quand sa cadence le permet" $ do
      let cad = unsafeRight "EngineSpec: cad" (mkCadence 1)
          v = vaisseauTest 100 100 3 2 cad
          pv = unsafeRight "EngineSpec: pv" (mkPV 2)
          h = unsafeRight "EngineSpec: h" (mkRectangle 100 700 30 34)
          e = unsafeRight "EngineSpec: e" (mkEnnemi h pv (Scripted [Attendre] 0) cad)
          (_, mp) = finDeTourEnnemiIntelligent 1 [v] e
      fmap prOwner mp `shouldBe` Just TirEnnemi
      fmap prDir mp `shouldBe` Just Bas
      fmap prJoueuse mp `shouldBe` Just Nothing

    prop "finDeTourEnnemiIntelligent respecte sa post-condition" $
      forAll genEnnemiSimple $ \e ->
        forAll (listOf genVaisseauActif) $ \js ->
          \graine -> prop_post_finDeTourEnnemiIntelligent graine js e (finDeTourEnnemiIntelligent graine js e)

  describe "Engine.EnnemiCollisions" $ do
    it "une collision ennemi/joueuse enlève 1 PV et pousse la joueuse vers le bas" $ do
      let cad = unsafeRight "EngineSpec: cad" (mkCadence 1)
          v = vaisseauTest 100 100 3 2 cad
          pv = unsafeRight "EngineSpec: pv" (mkPV 2)
          hEnnemi = unsafeRight "EngineSpec: hEnnemi" (mkRectangle 110 115 30 34)
          ennemi = unsafeRight "EngineSpec: ennemi" (mkEnnemi hEnnemi pv (Scripted [Attendre] 0) cad)
          m = unsafeRight "EngineSpec: m" (mkMoteurTest [] [] [ennemi] [v] cad [] 0 0)
          m' = resoudreCollisionsEnnemis m
      attendUneJoueuse (mJoueuses m') $ \v' -> do
        vjPv v' `shouldBe` 2
        snd (centreHitbox (vjHitbox v')) `shouldSatisfy` (< snd (centreHitbox (vjHitbox v)))

    it "un ennemi qui touche un obstacle est repoussé vers le haut" $ do
      let cad = unsafeRight "EngineSpec: cad" (mkCadence 1)
          pv = unsafeRight "EngineSpec: pv" (mkPV 2)
          hEnnemi = unsafeRight "EngineSpec: hEnnemi" (mkRectangle 200 300 30 34)
          ennemi = unsafeRight "EngineSpec: ennemi" (mkEnnemi hEnnemi pv (Scripted [Attendre] 0) cad)
          hObstacle = unsafeRight "EngineSpec: hObstacle" (mkRectangle 195 295 50 50)
          obstacle = unsafeRight "EngineSpec: obstacle" (mkObstacle hObstacle)
          v = vaisseauTest 100 100 3 2 cad
          m = unsafeRight "EngineSpec: m" (mkMoteurTest [obstacle] [] [ennemi] [v] cad [] 0 0)
          m' = resoudreCollisionsEnnemis m
      attendUnEnnemi (mEnnemis m') $ \ennemi' ->
        snd (centreHitbox (eHitbox ennemi')) `shouldSatisfy` (> snd (centreHitbox (eHitbox ennemi)))

    it "un ennemi qui touche un météore disparaît" $ do
      let cad = unsafeRight "EngineSpec: cad" (mkCadence 1)
          pv = unsafeRight "EngineSpec: pv" (mkPV 2)
          hEnnemi = unsafeRight "EngineSpec: hEnnemi" (mkRectangle 300 400 30 34)
          ennemi = unsafeRight "EngineSpec: ennemi" (mkEnnemi hEnnemi pv (Scripted [Attendre] 0) cad)
          meteore = unsafeRight "EngineSpec: meteore" (mkMeteoreRond 315 417 20 cad)
          v = vaisseauTest 100 100 3 2 cad
          m = unsafeRight "EngineSpec: m" (mkMoteurTest [] [] [ennemi] [v] cad [] 0 0)
          m' = resoudreCollisionsEnnemis (m { mMeteores = [meteore] })
      mEnnemis m' `shouldBe` []

    prop "resoudreCollisionsEnnemis respecte sa post-condition" $
      forAll genMoteur $ \m ->
        prop_post_resoudreCollisionsEnnemis m (resoudreCollisionsEnnemis m)

  describe "Engine.GameConfig" $ do
    it "la configuration par défaut crée une partie monojoueur valide" $ do
      let m = unsafeRight "EngineSpec: m" (mkMoteurPartie configPartieDefaut)
      length (mJoueuses m) `shouldBe` 1
      mObstacles m `shouldBe` []
      mProjectiles m `shouldBe` []
      mEnnemis m `shouldBe` []
      mMeteores m `shouldBe` []
      mScript m `shouldBe` []
      prop_inv_moteur m `shouldBe` True

    it "la configuration duo crée deux joueuses et deux scores" $ do
      let m = unsafeRight "EngineSpec: m" (mkMoteurPartie configPartieDuo)
      length (mJoueuses m) `shouldBe` 2
      length (mScores m) `shouldBe` 2
      prop_inv_moteur m `shouldBe` True

    it "mkJoueusesInitiales couvre MonoJoueur et DuoJoueurs" $ do
      let cad = unsafeRight "EngineSpec: cad" (mkCadence 1)
      fmap length (mkJoueusesInitiales MonoJoueur cad) `shouldBe` Right 1
      fmap length (mkJoueusesInitiales DuoJoueurs cad) `shouldBe` Right 2

    it "définit une cadence de tir valide selon le mode" $ do
      mkCadence (cadenceTirJoueuse configPartieDefaut) `shouldSatisfy` isRightWith prop_inv_cadence
      mkCadence (cadenceTirJoueuse configPartieDuo) `shouldSatisfy` isRightWith prop_inv_cadence

    it "exempleMoteur reste compatible avec la configuration par défaut" $ do
      let m1 = unsafeRight "EngineSpec: m1" (exempleMoteur)
          m2 = unsafeRight "EngineSpec: m2" (mkMoteurPartie configPartieDefaut)
      length (mJoueuses m1) `shouldBe` length (mJoueuses m2)
      prop_inv_moteur m1 `shouldBe` True

  describe "Engine.EnnemiSpawn et DynamicSpawn" $ do
    it "genererEnnemiSiBesoin n'ajoute pas d'ennemi hors période" $ do
      let cad = unsafeRight "EngineSpec: cad" (mkCadence 1)
          v = vaisseauTest 10 10 3 2 cad
          m = unsafeRight "EngineSpec: m" (mkMoteurTest [] [] [] [v] cad [] 1 0)
          m' = genererEnnemiSiBesoin m
      length (mEnnemis m') `shouldBe` 0
      prop_inv_moteur m' `shouldBe` True

    it "genererEnnemiSiBesoin ajoute un ennemi pendant une période d'apparition" $ do
      let cad = unsafeRight "EngineSpec: cad" (mkCadence 1)
          v = vaisseauTest 10 10 3 2 cad
          m0 = unsafeRight "EngineSpec: m0" (mkMoteurTest [] [] [] [v] cad [] periodeApparitionEnnemi 0)
          m' = genererEnnemiSiBesoin m0
      length (mEnnemis m') `shouldBe` 1
      prop_inv_moteur m' `shouldBe` True

    it "genererEnnemiSiBesoin ne dépasse pas le nombre maximal d'ennemis actifs" $ do
      let cad = unsafeRight "EngineSpec: cad" (mkCadence 1)
          v = vaisseauTest 10 10 3 2 cad
          pv = unsafeRight "EngineSpec: pv" (mkPV 2)
          h = unsafeRight "EngineSpec: h" (mkRectangle 200 700 30 34)
          ennemi = unsafeRight "EngineSpec: ennemi" (mkEnnemi h pv (Scripted [Attendre] 0) cad)
          ennemis = replicate maxEnnemisActifs ennemi
          m0 = unsafeRight "EngineSpec: m0" (mkMoteurTest [] [] ennemis [v] cad [] periodeApparitionEnnemi 0)
          m' = genererEnnemiSiBesoin m0
      length (mEnnemis m') `shouldBe` maxEnnemisActifs
      prop_inv_moteur m' `shouldBe` True

    prop "genererObjetsDynamiques respecte ses quotas et son invariant" $
      forAll genMoteur $ \m ->
        prop_post_genererObjetsDynamiques m (genererObjetsDynamiques m)

    it "la fréquence d'apparition augmente avec la survie" $ do
      periodeEnnemi 5000 `shouldSatisfy` (< periodeEnnemi 0)
      periodeMeteore 5000 `shouldSatisfy` (< periodeMeteore 0)
      periodeObstacle 5000 `shouldSatisfy` (< periodeObstacle 0)

    it "la génération dynamique ajoute au moins un objet après assez de tours" $ do
      let m0 = unsafeRight "EngineSpec: m0" (mkMoteurPartie configPartieDefaut)
          mFinal = iterate finDeTourMoteur m0 !! 600
          nbObjets = length (mEnnemis mFinal) + length (mMeteores mFinal) + length (mObstacles mFinal)
      nbObjets `shouldSatisfy` (> 0)
      prop_inv_moteur mFinal `shouldBe` True
