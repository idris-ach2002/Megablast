{- HLINT ignore "Use camelCase" -}
{-# LANGUAGE OverloadedStrings #-}

module Engine where

import Data.List  (partition, sortBy)
import Data.Ord   (comparing)
import Data.Text  (Text)
import Hitbox
import Objects
import System.Random (StdGen, mkStdGen, random)

-- Un événement planifié associe un numéro de tour à une action sur la scène.
-- Le script est une liste d'événements triée par tour croissant
data Evenement = 
  AppEnnemi    Ennemi        -- apparition d'un ennemi
  | AppObstacle  Obstacle      
  | AppProjectile Projectile   
  | DisparEnnemi   Int         -- disparition de l'ennemi à l'indice donné
  | DisparObstacle Int         
  deriving (Eq, Show)

-- Un événement planifié : le tour auquel il doit survenir + l'événement.
data EvenementPlanifie = EvenementPlanifie
  { 
    epTour     :: Int        -- doit être >= 0
  , epEvenement :: Evenement
  } deriving (Eq, Show)

prop_inv_evenement_planifie :: EvenementPlanifie -> Bool
prop_inv_evenement_planifie ep = epTour ep >= 0



-- | Le moteur maintient l'intégralité de l'état du jeu à un instant donné.
--
-- Invariant (prop_inv_moteur) :
--   * tous les obstacles, projectiles, ennemis et joueuses vérifient leur
--     propre invariant ;
--   * la cadence de défilement vérifie son invariant ;
--   * le script est trié par tour croissant, chaque événement vérifiant
--     son propre invariant ;
--   * mTour >= 0 ;
--   * il y a au moins une joueuse en vie (vjPv > 0 || vjEssais > 0) pour
--     que la partie continue (cette condition est vérifiée séparément par
--     prop_partie_en_cours).
data Moteur = Moteur
  { mObstacles   :: [Obstacle]
  , mProjectiles :: [Projectile]
  , mEnnemis     :: [Ennemi]
  , mJoueuses    :: [VaisseauJoueuse]
  , mCadScroll   :: Cadence            -- cadence du scrolling des obstacles
  , mScript      :: [EvenementPlanifie]-- événements futurs, triés par epTour
  , mTour        :: Int                -- numéro du tour courant (>= 0)
  , mRng         :: StdGen                -- graine pseudo-aléatoire pour les ennemie
  } deriving (Eq, Show)

prop_inv_moteur :: Moteur -> Bool
prop_inv_moteur m =
     all prop_inv_obstacle   (mObstacles   m)
  && all prop_inv_projectile  (mProjectiles m)
  && all prop_inv_ennemi      (mEnnemis     m)
  && all prop_inv_vaisseau    (mJoueuses    m)
  && prop_inv_cadence         (mCadScroll   m)
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

-- ---------------------------------------------------------------------------
--  Constructeur intelligent
-- ---------------------------------------------------------------------------

mkMoteur
  :: [Obstacle]
  -> [Projectile]
  -> [Ennemi]
  -> [VaisseauJoueuse]
  -> Cadence
  -> [EvenementPlanifie]   -- sera trié automatiquement
  -> Int                   -- tour initial (>= 0)
  -> Int                   -- graine RNG
  -> Either Text Moteur
mkMoteur obs projs enns jous cad evts tour seed
  | not (all prop_inv_obstacle obs)   = Left "mkMoteur: obstacle invalide"
  | not (all prop_inv_projectile projs) = Left "mkMoteur: projectile invalide"
  | not (all prop_inv_ennemi enns)    = Left "mkMoteur: ennemi invalide"
  | not (all prop_inv_vaisseau jous)  = Left "mkMoteur: vaisseau invalide"
  | not (prop_inv_cadence cad)        = Left "mkMoteur: cadence invalide"
  | tour < 0                          = Left "mkMoteur: tour negatif"
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

-- Application d'un événement du script
appliquerEvenement :: Evenement -> Moteur -> Moteur
appliquerEvenement (AppEnnemi e)      m = m { mEnnemis    = mEnnemis    m ++ [e] }
appliquerEvenement (AppObstacle o)    m = m { mObstacles  = mObstacles  m ++ [o] }
appliquerEvenement (AppProjectile p)  m = m { mProjectiles= mProjectiles m ++ [p] }
appliquerEvenement (DisparEnnemi i)   m = m { mEnnemis    = removeAt i (mEnnemis m) }
appliquerEvenement (DisparObstacle i) m = m { mObstacles  = removeAt i (mObstacles m) }

-- Suppression par indice (si hors-bornes, liste inchangée)
removeAt :: Int -> [a] -> [a]
removeAt _ []     = []
removeAt 0 (_:xs) = xs
removeAt n (x:xs) = x : removeAt (n-1) xs

-- ---------------------------------------------------------------------------
--Gestion des collisions
-- ---------------------------------------------------------------------------

-- | Joueuse touchée par un projectile ennemi → perd 1 PV.
--   Renvoie (joueuse mise à jour, projectile supprimé ?).
joueuseToucheePar :: VaisseauJoueuse -> Projectile -> (VaisseauJoueuse, Bool)
joueuseToucheePar v p
  | prOwner p == TirEnnemi
    && collision (vjHitbox v) (prHitbox p) = (subirDegat v, True)
  | otherwise                              = (v, False)

-- | Ennemi touché par un projectile joueuse → perd des PV (on retire l'ennemi
--   si ses PV atteignent 0).  Renvoie (ennemi ou Nothing, projectile consommé ?).
ennemiToucheePar :: Ennemi -> Projectile -> (Maybe Ennemi, Bool)
ennemiToucheePar e p
  | prOwner p == TirJoueuse
    && collision (eHitbox e) (prHitbox p) =
        let (PV pv) = ePV e
            pv'     = pv - 1
        in (if pv' > 0 then Just (e { ePV = PV pv' }) else Nothing, True)
  | otherwise = (Just e, False)

-- | Joueuse repoussée par un obstacle (collision détectée → recul vers le bas).
joueuseToucheeObstacle :: VaisseauJoueuse -> Obstacle -> VaisseauJoueuse
joueuseToucheeObstacle v o
  | collision (vjHitbox v) (obsHitbox o) = repousseVaisseau Haut v
  | otherwise                            = v

-- | Traite les collisions pour un tour :
--     1. projectiles ennemis vs joueuses
--     2. projectiles joueuses vs ennemis
--     3. joueuses vs obstacles
--   Renvoie le moteur avec les listes mises à jour.
resoudreCollisions :: Moteur -> Moteur
resoudreCollisions m =
  let
    -- 1. Projectiles ennemis vs chaque joueuse
    (jous1, projsApresJoueuses) =
      foldr appliquerProjSurJoueuses (mJoueuses m, []) (mProjectiles m)

    appliquerProjSurJoueuses p (js, acc) =
      let (js', consomme) = parcourirJoueuses p js
      in if consomme then (js', acc) else (js', p : acc)

    parcourirJoueuses _ []     = ([], False)
    parcourirJoueuses p (j:js) =
      let (j', hit) = joueuseToucheePar j p
      in if hit
           then (j' : js, True)
           else let (js', found) = parcourirJoueuses p js
                in (j : js', found)

    m1 = m { mJoueuses    = jous1
           , mProjectiles  = projsApresJoueuses }

    -- 2. Projectiles joueuses vs ennemis
    (enns2, projsRestes) =
      foldr appliquerProjSurEnnemis (mEnnemis m1, []) (mProjectiles m1)

    appliquerProjSurEnnemis p (es, acc) =
      let (es', consomme) = parcourirEnnemis p es
      in if consomme then (es', acc) else (es', p : acc)

    parcourirEnnemis _ []     = ([], False)
    parcourirEnnemis p (e:es) =
      let (me', hit) = ennemiToucheePar e p
          es'        = maybe es (:es) me'  -- remet l'ennemi s'il survit
      in if hit
           then (es', True)
           else let (es'', found) = parcourirEnnemis p es
                in (e : es'', found)

    m2 = m1 { mEnnemis     = enns2
             , mProjectiles = projsRestes }

    -- 3. Joueuses vs obstacles
    jous3 = [ foldl joueuseToucheeObstacle j (mObstacles m2)
            | j <- mJoueuses m2 ]

    m3 = m2 { mJoueuses = jous3 }

  in m3

-- ---------------------------------------------------------------------------
-- Propos  Fin de tour global
-- ---------------------------------------------------------------------------

-- Précondition
prop_pre_finDeTourMoteur :: Moteur -> Bool
prop_pre_finDeTourMoteur m =
  prop_inv_moteur m && prop_partie_en_cours m

-- Postcondition
prop_post_finDeTourMoteur :: Moteur -> Moteur -> Bool
prop_post_finDeTourMoteur m m' =
     prop_inv_moteur m'
  && mTour m' == mTour m + 1
  -- le nombre de joueuses est conservé
  && length (mJoueuses m') == length (mJoueuses m)
  -- tous les projectiles restants sont valides
  && all prop_inv_projectile (mProjectiles m')

-- | Avance le jeu d'un tour complet :
--   1. On consomme les événements du script dont le tour est <= mTour.
--   2. On fait avancer les projectiles (finDeTourProjectile).
--   3. On fait défiler les obstacles (finDeTourObstacles).
--   4. On fait jouer les ennemis (finDeTourEnnemi), récupérant leurs nouveaux
--      projectiles.
--   5. On supprime les projectiles sortis de l'écran (hors bornes grossières).
--   6. On résout les collisions.
--   7. On incrémente le tour et la graine RNG.
finDeTourMoteur :: Moteur -> Moteur
finDeTourMoteur m
  | not (prop_inv_moteur m) = error "finDeTourMoteur: invariant moteur violé"
  | otherwise =
      let tourActuel = mTour m

          -- 1. Événements du script prévus pour ce tour (ou avant)
          (evtsNow, evtsFutur) =
            partition (\ep -> epTour ep <= tourActuel) (mScript m)
          m0 = foldr (appliquerEvenement . epEvenement) m evtsNow
          m1 = m0 { mScript = evtsFutur }

          -- 2. Avancement des projectiles existants
          projs' = map finDeTourProjectile (mProjectiles m1)

          -- 3. Défilement des obstacles
          (cadScroll', obs') = finDeTourObstacles (mCadScroll m1) (mObstacles m1)

          -- 4. Tour des ennemis : actions + nouveaux projectiles
          rng = mRng m1
          (graineEnnemis, rngNext) = random rng :: (Int, StdGen)

          (enns', newProjs) = unzip $ map (finDeTourEnnemi graineEnnemis) (mEnnemis m1)
          projsEnnemis      = [ p | Just p <- newProjs ]

          -- 5. Assemblage et filtrage des projectiles hors écran
          tousProjs = projs' ++ projsEnnemis
          projsValides = filter (not . horsEcranGrossier . prHitbox) tousProjs

          -- 6. Collisions
          m2 = resoudreCollisions
                 (m1 { mObstacles   = obs'
                     , mProjectiles = projsValides
                     , mEnnemis     = enns'
                     , mCadScroll   = cadScroll'
                     })

          -- 7. Incréments
          m3 = m2 { mTour = tourActuel + 1
                  , mRng  = rngNext }

      in m3
      
-- | Heuristique : une hitbox est considérée hors-écran si elle dépasse les
--   bornes [-100, 2000] sur chaque axe.  On ne connaît pas la taille exacte
--   de l'écran dans le moteur ; les parties supérieures/inférieures à ces
--   valeurs ne sont de toute façon jamais visibles.
horsEcranGrossier :: Hitbox -> Bool
horsEcranGrossier (Point x y)         = x < -100 || x > 2000 || y < -100 || y > 2000
horsEcranGrossier (Disque xc yc r)    = yc + r < -100 || yc - r > 2000
horsEcranGrossier (Rectangle x y w h) = x + w < -100 || x > 2000
                                      || y + h < -100 || y > 2000
horsEcranGrossier (Composee hs)       = all horsEcranGrossier hs
horsEcranGrossier _                   = False  -- murs : toujours présents

-- ---------------------------------------------------------------------------
-- Gestion des commandes joueuse
-- ---------------------------------------------------------------------------

-- | Applique une commande à la joueuse d'indice i dans le moteur.
--   Renvoie le moteur mis à jour (et le projectile créé si c'est un Tir).
appliquerCommande :: Int -> Action -> Cadence -> Moteur -> (Moteur, Maybe Projectile)
appliquerCommande i Attendre _ m = (m, Nothing)
appliquerCommande i (Deplacer d) _ m =
  let jous = mJoueuses m
      v    = jous !! i
      v'   = deplaceVaisseau d v
      -- Repousser si collision avec un mur ou un obstacle après déplacement
      v'' = foldl joueuseToucheeObstacle v' (mObstacles m)
  in (m { mJoueuses = replaceAt i v'' jous }, Nothing)
appliquerCommande i Tirer cadTir m =
  let jous = mJoueuses m
      v    = jous !! i
      p    = tirVaisseau v cadTir
  in (m { mProjectiles = mProjectiles m ++ [p] }, Just p)

replaceAt :: Int -> a -> [a] -> [a]
replaceAt _ _ []     = []
replaceAt 0 x (_:xs) = x : xs
replaceAt n x (y:ys) = y : replaceAt (n-1) x ys

