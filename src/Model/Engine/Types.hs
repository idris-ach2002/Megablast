{- HLINT ignore "Use camelCase" -}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE InstanceSigs #-}

module Model.Engine.Types where

import Data.List (sortBy)
import Data.Ord (comparing)
import Data.Text (Text)
import Model.Hitbox
import Model.Meteore
import Model.Objects
import System.Random (StdGen, mkStdGen)
import Model.Score

---------------------------------------------------------------------------------
-- Script du moteur
---------------------------------------------------------------------------------

-- | Script séquentiel du moteur.
--
--   Ce type rend explicite la structure algébrique du script :
--   * Functor/Applicative/Monad pour transformer et composer des scénarios,
--   * Semigroup/Monoid pour concaténer des fragments de script,
--   * Foldable/Traversable pour parcourir/valider les événements.
newtype Script a = Script
  { scriptToList :: [a]
  } deriving (Eq, Show)

scriptFromList :: [a] -> Script a
scriptFromList = Script

instance Functor Script where
  fmap :: (a -> b) -> Script a -> Script b
  fmap f (Script xs) = Script (map f xs)

instance Applicative Script where
  pure :: a -> Script a
  pure x = Script [x]

  (<*>) :: Script (a -> b) -> Script a -> Script b
  Script fs <*> Script xs = Script [f x | f <- fs, x <- xs]

instance Monad Script where
  (>>=) :: Script a -> (a -> Script b) -> Script b
  Script xs >>= f = Script (concatMap (scriptToList . f) xs)

instance Foldable Script where
  foldMap :: Monoid m => (a -> m) -> Script a -> m
  foldMap f (Script xs) = foldMap f xs

instance Traversable Script where
  traverse :: Applicative f => (a -> f b) -> Script a -> f (Script b)
  traverse f (Script xs) = Script <$> traverse f xs

instance Semigroup (Script a) where
  (<>) :: Script a -> Script a -> Script a
  Script xs <> Script ys = Script (xs <> ys)

instance Monoid (Script a) where
  mempty :: Script a
  mempty = Script []

ordonnerScript :: Script EvenementPlanifie -> Script EvenementPlanifie
ordonnerScript (Script evts) =
  Script (sortBy (comparing epTour) evts)

partitionScript :: (a -> Bool) -> Script a -> (Script a, Script a)
partitionScript p (Script xs) =
  let (ys, zs) = partitionList p xs
  in (Script ys, Script zs)
  where
    partitionList _ [] = ([], [])
    partitionList q (x:rest) =
      let (as, bs) = partitionList q rest
      in if q x then (x:as, bs) else (as, x:bs)

scriptTrieParTour :: Script EvenementPlanifie -> Bool
scriptTrieParTour (Script [])       = True
scriptTrieParTour (Script [_])      = True
scriptTrieParTour (Script (a:b:xs)) =
  epTour a <= epTour b && scriptTrieParTour (Script (b:xs))

prop_script_functor_id :: Eq a => Script a -> Bool
prop_script_functor_id script =
  fmap id script == script

prop_script_semigroup_assoc :: Eq a => Script a -> Script a -> Script a -> Bool
prop_script_semigroup_assoc x y z =
  x <> (y <> z) == (x <> y) <> z

prop_script_monoid_left :: Eq a => Script a -> Bool
prop_script_monoid_left x =
  mempty <> x == x

prop_script_monoid_right :: Eq a => Script a -> Bool
prop_script_monoid_right x =
  x <> mempty == x

---------------------------------------------------------------------------------
-- Événements
---------------------------------------------------------------------------------

-- | Un événement planifié associe un numéro de tour à une action sur la scène.
--   Le script du moteur est une séquence d'événements triée par tour croissant.
data Evenement =
    AppEnnemi Ennemi
  | AppObstacle Obstacle
  | AppProjectile Projectile
  | AppMeteore Meteore
  | DisparEnnemi Int
  | DisparObstacle Int
  | DisparMeteore Int
  deriving (Eq, Show)

-- | Un événement du script avec son tour de déclenchement.
data EvenementPlanifie = EvenementPlanifie
  { epTour      :: Int
  , epEvenement :: Evenement
  } deriving (Eq, Show)

prop_inv_evenement_planifie :: EvenementPlanifie -> Bool
prop_inv_evenement_planifie ep = epTour ep >= 0

-- | Dimensions logiques utilisées par le moteur.
largeurZoneJeu, hauteurZoneJeu, margeHorsEcran :: Int
largeurZoneJeu = 800
hauteurZoneJeu = 900
margeHorsEcran = 100

---------------------------------------------------------------------------------
-- Murs du niveau
---------------------------------------------------------------------------------

data MursNiveau = MursNiveau
  { mnMurGauche :: Hitbox
  , mnMurDroit  :: Hitbox
  } deriving (Eq, Show)

estMurGauche :: Hitbox -> Bool
estMurGauche (MurGauche _) = True
estMurGauche _             = False

estMurDroit :: Hitbox -> Bool
estMurDroit (MurDroit _) = True
estMurDroit _            = False

prop_inv_mursNiveau :: MursNiveau -> Bool
prop_inv_mursNiveau murs =
     estMurGauche (mnMurGauche murs)
  && estMurDroit  (mnMurDroit murs)
  && prop_inv_hitbox (mnMurGauche murs)
  && prop_inv_hitbox (mnMurDroit murs)

---------------------------------------------------------------------------------
-- Moteur
---------------------------------------------------------------------------------

-- | Le moteur maintient l'intégralité de l'état du jeu à un instant donné.
data Moteur = Moteur
  { mObstacles   :: [Obstacle]
  , mProjectiles :: [Projectile]
  , mEnnemis     :: [Ennemi]
  , mMeteores    :: [Meteore]
  , mJoueuses    :: [VaisseauJoueuse]
  , mMurs        :: MursNiveau
  , mCadScroll   :: Cadence
  , mScript      :: Script EvenementPlanifie
  , mTour        :: Int
  , mRng         :: StdGen
  , mScores      :: [Score]
  , mScore       :: Score
  } deriving (Eq, Show)

prop_inv_moteur :: Moteur -> Bool
prop_inv_moteur m =
     all prop_inv_obstacle           (mObstacles m)
  && all prop_inv_projectile         (mProjectiles m)
  && all prop_inv_ennemi             (mEnnemis m)
  && all prop_inv_meteore            (mMeteores m)
  && all prop_inv_vaisseau           (mJoueuses m)
  && prop_inv_mursNiveau             (mMurs m)
  && prop_inv_cadence                (mCadScroll m)
  && prop_inv_scores                 (mScores m)
  && prop_inv_score                  (mScore m)
  && scoreTotal (mScores m) == mScore m
  && mTour m >= 0
  && scriptTrieParTour               (mScript m)
  && all prop_inv_evenement_planifie (mScript m)

mkMoteur
  :: [Obstacle]
  -> [Projectile]
  -> [Ennemi]
  -> [VaisseauJoueuse]
  -> MursNiveau
  -> Cadence
  -> [EvenementPlanifie]
  -> Int
  -> Int
  -> Either Text Moteur
mkMoteur obs projs enns jous murs cad evts tour seed
  | not (all prop_inv_obstacle obs)      = Left "mkMoteur: obstacle invalide"
  | not (all prop_inv_projectile projs)  = Left "mkMoteur: projectile invalide"
  | not (all prop_inv_ennemi enns)       = Left "mkMoteur: ennemi invalide"
  | not (all prop_inv_vaisseau jous)     = Left "mkMoteur: vaisseau invalide"
  | not (prop_inv_mursNiveau murs)       = Left "mkMoteur: murs invalides"
  | not (prop_inv_cadence cad)           = Left "mkMoteur: cadence invalide"
  | tour < 0                             = Left "mkMoteur: tour negatif"
  | not (all prop_inv_evenement_planifie evts)
                                         = Left "mkMoteur: evenement invalide"
  | otherwise = Right Moteur
      { mObstacles   = obs
      , mProjectiles = projs
      , mEnnemis     = enns
      , mMeteores    = []
      , mJoueuses    = jous
      , mMurs        = murs
      , mCadScroll   = cad
      , mScript      = ordonnerScript (scriptFromList evts)
      , mTour        = tour
      , mRng         = mkStdGen seed
      , mScores      = scoresNuls (length jous)
      , mScore       = scoreNul
      }
