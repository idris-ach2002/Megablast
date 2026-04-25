module Model.Engine.Commands where

import Model.Engine.Collisions
import Model.Engine.ListUtils
import Model.Engine.Types
import Model.Objects

-- | Précondition naturelle d'application d'une commande : indice valide,
--   moteur valide, cadence de tir valide.
prop_pre_appliquerCommande :: Int -> Action -> Cadence -> Moteur -> Bool
prop_pre_appliquerCommande i _ cad m =
  prop_inv_moteur m && prop_inv_cadence cad && i >= 0 && i < length (mJoueuses m)

prop_post_appliquerCommande :: Int -> Action -> Cadence -> Moteur -> (Moteur, Maybe Projectile) -> Bool
prop_post_appliquerCommande _ _ _ _ (m', mp) =
  prop_inv_moteur m' && maybe True prop_inv_projectile mp

-- | Applique une commande à la joueuse d'indice i dans le moteur.
--   Si l'indice est invalide, on ne modifie pas le moteur : le contrat est
--   explicite et la fonction n'est plus partielle.
appliquerCommande :: Int -> Action -> Cadence -> Moteur -> (Moteur, Maybe Projectile)
appliquerCommande _ Attendre _ m = (m, Nothing)
appliquerCommande i (Deplacer d) _ m =
  case lookupAt i (mJoueuses m) of
    Nothing -> (m, Nothing)
    Just v
      | not (joueuseEncoreEnJeu v) -> (m, Nothing)
      | otherwise ->
          let v'   = essaieDeplacerVaisseau d v
              v''  = foldl joueuseToucheeObstacle v' (mObstacles m)
              v''' = joueuseToucheeMurs (mMurs m) v''
          in (m { mJoueuses = replaceAt i v''' (mJoueuses m) }, Nothing)

appliquerCommande i Tirer cadTir m =
  case lookupAt i (mJoueuses m) of
    Nothing -> (m, Nothing)
    Just v
      | not (joueuseEncoreEnJeu v) -> (m, Nothing)
      | otherwise ->
          let p = tirVaisseau v cadTir
          in (m { mProjectiles = mProjectiles m ++ [p] }, Just p)
