{-# LANGUAGE OverloadedStrings #-}

module Model.Engine.EnnemiCollisions where

import Model.Engine.Collisions
import Model.Engine.Types
import Model.Hitbox
import Model.Meteore
import Model.Objects

---------------------------------------------------------------------------------
-- Collisions spéciales des ennemis
---------------------------------------------------------------------------------

distanceRepousseJoueuseParEnnemi :: Int
distanceRepousseJoueuseParEnnemi = 35

distanceRepousseEnnemiObstacle :: Int
distanceRepousseEnnemiObstacle = 30

-- | Résout les collisions propres aux ennemis :
--   - météore / ennemi : l'ennemi disparaît ;
--   - obstacle / ennemi : l'ennemi est repoussé vers le haut ;
--   - ennemi / joueuse : la joueuse prend un dégât et est poussée vers le bas.
resoudreCollisionsEnnemis :: Moteur -> Moteur
resoudreCollisionsEnnemis m =
  let
    ennemisSansMeteore =
      filter (not . ennemiToucheUnMeteore (mMeteores m)) (mEnnemis m)

    ennemisRepousses =
      map (repousserEnnemiParObstacles (mObstacles m)) ennemisSansMeteore

    joueusesApresContact =
      map (joueuseToucheeParEnnemis ennemisRepousses) (mJoueuses m)

  in m { mEnnemis  = ennemisRepousses
       , mJoueuses = supprimerJoueusesEliminees joueusesApresContact
       }

---------------------------------------------------------------------------------
-- Ennemi / météore
---------------------------------------------------------------------------------

ennemiToucheUnMeteore :: [Meteore] -> Ennemi -> Bool
ennemiToucheUnMeteore meteores ennemi =
  any (ennemiToucheMeteore ennemi) meteores

ennemiToucheMeteore :: Ennemi -> Meteore -> Bool
ennemiToucheMeteore ennemi meteore =
  collision (eHitbox ennemi) (mtHitbox meteore)

---------------------------------------------------------------------------------
-- Ennemi / obstacle
---------------------------------------------------------------------------------

repousserEnnemiParObstacles :: [Obstacle] -> Ennemi -> Ennemi
repousserEnnemiParObstacles obstacles ennemi
  | any (ennemiToucheObstacle ennemi) obstacles =
      pousserEnnemi distanceRepousseEnnemiObstacle Haut ennemi
  | otherwise =
      ennemi

ennemiToucheObstacle :: Ennemi -> Obstacle -> Bool
ennemiToucheObstacle ennemi obstacle =
  collision (eHitbox ennemi) (obsHitbox obstacle)

pousserEnnemi :: Int -> Direction -> Ennemi -> Ennemi
pousserEnnemi n direction ennemi
  | n <= 0 =
      ennemi
  | otherwise =
      pousserEnnemi (n - 1) direction (deplacerEnnemiDirect direction ennemi)

deplacerEnnemiDirect :: Direction -> Ennemi -> Ennemi
deplacerEnnemiDirect direction ennemi =
  let (dx, dy) = dirVector direction
  in ennemi { eHitbox = translateHitbox dx dy (eHitbox ennemi) }

---------------------------------------------------------------------------------
-- Ennemi / joueuse
---------------------------------------------------------------------------------

joueuseToucheeParEnnemis :: [Ennemi] -> VaisseauJoueuse -> VaisseauJoueuse
joueuseToucheeParEnnemis ennemis joueuse =
  foldl joueuseToucheeParEnnemi joueuse ennemis

joueuseToucheeParEnnemi :: VaisseauJoueuse -> Ennemi -> VaisseauJoueuse
joueuseToucheeParEnnemi joueuse ennemi
  | not (joueuseEncoreEnJeu joueuse) =
      joueuse
  | collision (vjHitbox joueuse) (eHitbox ennemi) =
      pousserJoueuse
        distanceRepousseJoueuseParEnnemi
        Bas
        (encaisserDegatJoueuse joueuse)
  | otherwise =
      joueuse

pousserJoueuse :: Int -> Direction -> VaisseauJoueuse -> VaisseauJoueuse
pousserJoueuse n direction joueuse
  | n <= 0 =
      joueuse
  | otherwise =
      pousserJoueuse (n - 1) direction (deplaceVaisseau direction joueuse)

prop_pre_resoudreCollisionsEnnemis :: Moteur -> Bool
prop_pre_resoudreCollisionsEnnemis =
  prop_inv_moteur

prop_post_resoudreCollisionsEnnemis :: Moteur -> Moteur -> Bool
prop_post_resoudreCollisionsEnnemis m m' =
     prop_inv_moteur m'
  && length (mEnnemis m') <= length (mEnnemis m)
  && length (mJoueuses m') <= length (mJoueuses m)