{- HLINT ignore "Use camelCase" -}
{-# LANGUAGE OverloadedStrings #-}

module Model.Engine where

import Data.List  (partition, sortBy)
import Data.Ord   (comparing)
import Data.Text  (Text)
import Model.Hitbox
import Model.Objects
import System.Random (StdGen, mkStdGen, random)

-- Un événement planifié associe un numéro de tour à une action sur la scène.
-- Le script est une liste d'événements triée par tour croissant.
data Evenement =
    AppEnnemi    Ennemi
  | AppObstacle  Obstacle
  | AppProjectile Projectile
  | DisparEnnemi   Int
  | DisparObstacle Int
  deriving (Eq, Show)

-- Un événement planifié : le tour auquel il doit survenir + l'événement.
data EvenementPlanifie = EvenementPlanifie
  {
    epTour      :: Int
  , epEvenement :: Evenement
  } deriving (Eq, Show)

prop_inv_evenement_planifie :: EvenementPlanifie -> Bool
prop_inv_evenement_planifie ep = epTour ep >= 0

-- | Dimensions logiques utilisées par le moteur pour déterminer quand un
-- objet a quitté la zone de jeu. On évite ainsi les nombres magiques.
largeurZoneJeu, hauteurZoneJeu, margeHorsEcran :: Int
largeurZoneJeu = 800
hauteurZoneJeu = 900
margeHorsEcran = 100

-- | Le moteur maintient l'intégralité de l'état du jeu à un instant donné.
data Moteur = Moteur
  { mObstacles   :: [Obstacle]
  , mProjectiles :: [Projectile]
  , mEnnemis     :: [Ennemi]
  , mJoueuses    :: [VaisseauJoueuse]
  , mCadScroll   :: Cadence
  , mScript      :: [EvenementPlanifie]
  , mTour        :: Int
  , mRng         :: StdGen
  } deriving (Eq, Show)

prop_inv_moteur :: Moteur -> Bool
prop_inv_moteur m =
     all prop_inv_obstacle         (mObstacles m)
  && all prop_inv_projectile       (mProjectiles m)
  && all prop_inv_ennemi           (mEnnemis m)
  && all prop_inv_vaisseau         (mJoueuses m)
  && prop_inv_cadence              (mCadScroll m)
  && mTour m >= 0
  && scriptTrie (mScript m)
  && all prop_inv_evenement_planifie (mScript m)
  where
    scriptTrie []       = True
    scriptTrie [_]      = True
    scriptTrie (a:b:xs) = epTour a <= epTour b && scriptTrie (b:xs)

prop_partie_en_cours :: Moteur -> Bool
prop_partie_en_cours m =
  any (\v -> vjPv v > 0 || vjEssais v > 0) (mJoueuses m)

mkMoteur
  :: [Obstacle]
  -> [Projectile]
  -> [Ennemi]
  -> [VaisseauJoueuse]
  -> Cadence
  -> [EvenementPlanifie]
  -> Int
  -> Int
  -> Either Text Moteur
mkMoteur obs projs enns jous cad evts tour seed
  | not (all prop_inv_obstacle obs)      = Left "mkMoteur: obstacle invalide"
  | not (all prop_inv_projectile projs)  = Left "mkMoteur: projectile invalide"
  | not (all prop_inv_ennemi enns)       = Left "mkMoteur: ennemi invalide"
  | not (all prop_inv_vaisseau jous)     = Left "mkMoteur: vaisseau invalide"
  | not (prop_inv_cadence cad)           = Left "mkMoteur: cadence invalide"
  | tour < 0                             = Left "mkMoteur: tour negatif"
  | not (all prop_inv_evenement_planifie evts)
                                         = Left "mkMoteur: evenement invalide"
  | otherwise = Right Moteur
      { mObstacles   = obs
      , mProjectiles = projs
      , mEnnemis     = enns
      , mJoueuses    = jous
      , mCadScroll   = cad
      , mScript      = sortBy (comparing epTour) evts
      , mTour        = tour
      , mRng         = mkStdGen seed
      }

-- | Application d'un événement du script.
appliquerEvenement :: Evenement -> Moteur -> Moteur
appliquerEvenement (AppEnnemi e)      m = m { mEnnemis     = mEnnemis m ++ [e] }
appliquerEvenement (AppObstacle o)    m = m { mObstacles   = mObstacles m ++ [o] }
appliquerEvenement (AppProjectile p)  m = m { mProjectiles = mProjectiles m ++ [p] }
appliquerEvenement (DisparEnnemi i)   m = m { mEnnemis     = removeAt i (mEnnemis m) }
appliquerEvenement (DisparObstacle i) m = m { mObstacles   = removeAt i (mObstacles m) }

-- | Suppression par indice. Si l'indice est invalide, la liste est inchangée.
removeAt :: Int -> [a] -> [a]
removeAt i xs
  | i < 0     = xs
removeAt _ [] = []
removeAt 0 (_:rest) = rest
removeAt n (x:rest) = x : removeAt (n - 1) rest

-- | Recherche sûre d'un élément par indice.
lookupAt :: Int -> [a] -> Maybe a
lookupAt i _      | i < 0 = Nothing
lookupAt _ []               = Nothing
lookupAt 0 (x:_)            = Just x
lookupAt n (_:xs)           = lookupAt (n - 1) xs

-- | Remplacement sûr d'un élément par indice.
replaceAt :: Int -> a -> [a] -> [a]
replaceAt i x xs
  | i < 0     = xs
replaceAt _ _ [] = []
replaceAt 0 x (_:xs) = x : xs
replaceAt n x (y:ys) = y : replaceAt (n - 1) x ys

pvRespawnMinimal :: Int
pvRespawnMinimal = 1

reapparaitreJoueuse :: VaisseauJoueuse -> VaisseauJoueuse
reapparaitreJoueuse v
  | vjPv v > 0      = v
  | vjEssais v <= 0 = v
  | otherwise       = v { vjPv = pvRespawnMinimal, vjEssais = vjEssais v - 1 }

-- | Applique un dégât à une joueuse puis gère immédiatement la transition vers
-- l'état "essai consommé / respawn" quand c'est nécessaire.
encaisserDegatJoueuse :: VaisseauJoueuse -> VaisseauJoueuse
encaisserDegatJoueuse = reapparaitreJoueuse . subirDegat

joueuseToucheePar :: VaisseauJoueuse -> Projectile -> (VaisseauJoueuse, Bool)
joueuseToucheePar v p
  | prOwner p == TirEnnemi
    && collision (vjHitbox v) (prHitbox p) = (encaisserDegatJoueuse v, True)
  | otherwise                              = (v, False)

ennemiToucheePar :: Ennemi -> Projectile -> (Maybe Ennemi, Bool)
ennemiToucheePar e p
  | prOwner p == TirJoueuse
    && collision (eHitbox e) (prHitbox p) =
        let (PV pv) = ePV e
            pv' = pv - 1
        in (if pv' > 0 then Just (e { ePV = PV pv' }) else Nothing, True)
  | otherwise = (Just e, False)

joueuseToucheeObstacle :: VaisseauJoueuse -> Obstacle -> VaisseauJoueuse
joueuseToucheeObstacle v o
  | collision (vjHitbox v) (obsHitbox o) = repousseVaisseau Haut v
  | otherwise                            = v

joueuseToucheeEnnemi :: VaisseauJoueuse -> Ennemi -> VaisseauJoueuse
joueuseToucheeEnnemi v e
  | collision (vjHitbox v) (eHitbox e) = repousseVaisseau Haut (encaisserDegatJoueuse v)
  | otherwise                          = v

resoudreCollisions :: Moteur -> Moteur
resoudreCollisions m =
  let
    (jous1, projsApresJoueuses) =
      foldr appliquerProjSurJoueuses (mJoueuses m, []) (mProjectiles m)

    appliquerProjSurJoueuses p (js, acc) =
      let (js', consomme) = parcourirJoueuses p js
      in if consomme then (js', acc) else (js', p : acc)

    parcourirJoueuses _ [] = ([], False)
    parcourirJoueuses p (j:js) =
      let (j', hit) = joueuseToucheePar j p
      in if hit
           then (j' : js, True)
           else let (js', found) = parcourirJoueuses p js
                in (j : js', found)

    m1 = m { mJoueuses = jous1, mProjectiles = projsApresJoueuses }

    (enns2, projsRestes) =
      foldr appliquerProjSurEnnemis (mEnnemis m1, []) (mProjectiles m1)

    appliquerProjSurEnnemis p (es, acc) =
      let (es', consomme) = parcourirEnnemis p es
      in if consomme then (es', acc) else (es', p : acc)

    parcourirEnnemis _ [] = ([], False)
    parcourirEnnemis p (e:es) =
      let (me', hit) = ennemiToucheePar e p
          es' = maybe es (:es) me'
      in if hit
           then (es', True)
           else let (es'', found) = parcourirEnnemis p es
                in (e : es'', found)

    m2 = m1 { mEnnemis = enns2, mProjectiles = projsRestes }
    jous3 = [ foldl joueuseToucheeObstacle j (mObstacles m2) | j <- mJoueuses m2 ]
    jous4 = [ foldl joueuseToucheeEnnemi   j (mEnnemis m2)   | j <- jous3 ]
  in m2 { mJoueuses = jous4 }

prop_pre_finDeTourMoteur :: Moteur -> Bool
prop_pre_finDeTourMoteur m =
  prop_inv_moteur m && prop_partie_en_cours m

prop_post_finDeTourMoteur :: Moteur -> Moteur -> Bool
prop_post_finDeTourMoteur m m' =
     prop_inv_moteur m'
  && mTour m' == mTour m + 1
  && length (mJoueuses m') == length (mJoueuses m)
  && all prop_inv_projectile (mProjectiles m')

-- | Version sûre de la fin de tour. Elle rend explicite l'échec par `Either`
-- au lieu de supposer implicitement que l'invariant est déjà respecté.
finDeTourMoteurEither :: Moteur -> Either Text Moteur
finDeTourMoteurEither m
  | not (prop_inv_moteur m) = Left "finDeTourMoteur: invariant moteur violé"
  | otherwise =
      let tourActuel = mTour m
          (evtsNow, evtsFutur) = partition (\ep -> epTour ep <= tourActuel) (mScript m)
          m0 = foldr (appliquerEvenement . epEvenement) m evtsNow
          m1 = m0 { mScript = evtsFutur }

          projs' = map finDeTourProjectile (mProjectiles m1)
          (cadScroll', obs') = finDeTourObstacles (mCadScroll m1) (mObstacles m1)

          rng = mRng m1
          (graineEnnemis, rngNext) = random rng :: (Int, StdGen)
          (enns', newProjs) = unzip $ map (finDeTourEnnemi graineEnnemis) (mEnnemis m1)
          projsEnnemis = [ p | Just p <- newProjs ]

          tousProjs = projs' ++ projsEnnemis
          projsValides = filter (not . horsEcranMoteur . prHitbox) tousProjs

          m2 = resoudreCollisions
                (m1 { mObstacles   = obs'
                    , mProjectiles = projsValides
                    , mEnnemis     = enns'
                    , mCadScroll   = cadScroll'
                    })

          m3 = m2 { mTour = tourActuel + 1, mRng = rngNext }
      in Right m3

-- | Version historique conservée pour le reste du code. En cas d'entrée
-- invalide, on renvoie l'état inchangé : la version `Either` ci-dessus est à
-- privilégier quand on veut rendre le contrat explicite.
finDeTourMoteur :: Moteur -> Moteur
finDeTourMoteur m =
  case finDeTourMoteurEither m of
    Right m' -> m'
    Left _   -> m

-- | Détermine si une hitbox est hors de l'écran logique du moteur.
-- Les murs ne sont pas supprimés, car ils définissent les bords du niveau.
horsEcranHitbox :: Int -> Int -> Int -> Hitbox -> Bool
horsEcranHitbox largeur hauteur marge (Point x y) =
  x < (-marge) || x > largeur + marge || y < (-marge) || y > hauteur + marge
horsEcranHitbox largeur hauteur marge (Disque xc yc r) =
  xc + r < (-marge) || xc - r > largeur + marge
  || yc + r < (-marge) || yc - r > hauteur + marge
horsEcranHitbox largeur hauteur marge (Rectangle x y w h) =
  x + w < (-marge) || x > largeur + marge
  || y + h < (-marge) || y > hauteur + marge
horsEcranHitbox largeur hauteur marge (Composee hs) =
  all (horsEcranHitbox largeur hauteur marge) hs
horsEcranHitbox _ _ _ _ = False

horsEcranMoteur :: Hitbox -> Bool
horsEcranMoteur = horsEcranHitbox largeurZoneJeu hauteurZoneJeu margeHorsEcran

-- | Précondition naturelle d'application d'une commande : indice valide,
-- moteur valide, cadence de tir valide.
prop_pre_appliquerCommande :: Int -> Action -> Cadence -> Moteur -> Bool
prop_pre_appliquerCommande i _ cad m =
  prop_inv_moteur m && prop_inv_cadence cad && i >= 0 && i < length (mJoueuses m)

prop_post_appliquerCommande :: Int -> Action -> Cadence -> Moteur -> (Moteur, Maybe Projectile) -> Bool
prop_post_appliquerCommande _ _ _ _ (m', mp) =
  prop_inv_moteur m' && maybe True prop_inv_projectile mp

-- | Applique une commande à la joueuse d'indice i dans le moteur.
-- Si l'indice est invalide, on ne modifie pas le moteur : le contrat est donc
-- explicite et la fonction n'est plus partielle.
appliquerCommande :: Int -> Action -> Cadence -> Moteur -> (Moteur, Maybe Projectile)
appliquerCommande _ Attendre _ m = (m, Nothing)
appliquerCommande i (Deplacer d) _ m =
  case lookupAt i (mJoueuses m) of
    Nothing -> (m, Nothing)
    Just v  ->
      let v' = deplaceVaisseau d v
          v'' = foldl joueuseToucheeObstacle v' (mObstacles m)
      in (m { mJoueuses = replaceAt i v'' (mJoueuses m) }, Nothing)
appliquerCommande i Tirer cadTir m =
  case lookupAt i (mJoueuses m) of
    Nothing -> (m, Nothing)
    Just v  ->
      let p = tirVaisseau v cadTir
      in (m { mProjectiles = mProjectiles m ++ [p] }, Just p)

exempleMoteur :: Either Text Moteur
exempleMoteur = do
  cad <- mkCadence 3
  hVaisseau <- mkRectangle 50 20 10 10
  cadV <- mkCadence 1
  vaisseau <- mkVaisseauJoueuse hVaisseau 1 2 cadV
  vaisseau2 <- mkVaisseauJoueuse hVaisseau 1 2 cadV
  hObs <- mkRectangle 30 300 40 20
  obs <- mkObstacle hObs
  let oracle = Scripted [Attendre, Deplacer Gauche, Tirer] 0
  pv <- mkPV 3
  cadEnn <- mkCadence 2
  hEnn <- mkRectangle 60 500 12 12
  enn <- mkEnnemi hEnn pv oracle cadEnn
  mkMoteur
    [obs]
    []
    [enn]
    [vaisseau, vaisseau2]
    cad
    [ EvenementPlanifie 5  (AppObstacle obs)
    , EvenementPlanifie 10 (DisparEnnemi 0)
    ]
    0
    42

prop_tour_incremente :: Moteur -> Bool
prop_tour_incremente m
  | not (prop_inv_moteur m)   = True
  | not (prop_partie_en_cours m) = True
  | otherwise = mTour (finDeTourMoteur m) == mTour m + 1

prop_invariant_preserve :: Moteur -> Bool
prop_invariant_preserve m
  | not (prop_inv_moteur m)   = True
  | not (prop_partie_en_cours m) = True
  | otherwise = prop_inv_moteur (finDeTourMoteur m)

prop_joueuses_stables :: Moteur -> Bool
prop_joueuses_stables m
  | not (prop_inv_moteur m)   = True
  | not (prop_partie_en_cours m) = True
  | otherwise = length (mJoueuses (finDeTourMoteur m)) == length (mJoueuses m)

prop_script_diminue :: Moteur -> Bool
prop_script_diminue m
  | not (prop_inv_moteur m)   = True
  | not (prop_partie_en_cours m) = True
  | otherwise =
      let m' = finDeTourMoteur m
          t' = mTour m'
      in all (\ep -> epTour ep > t') (mScript m')

-- | Preuve informelle de prop_tour_incremente :
--
--   Soit m un moteur vérifiant prop_inv_moteur et prop_partie_en_cours.
--   Par définition de finDeTourMoteur, la dernière étape pose :
--     mTour m' = mTour m + 1
--   Donc mTour (finDeTourMoteur m) = mTour m + 1.
