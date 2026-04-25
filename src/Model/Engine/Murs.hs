{- HLINT ignore "Use camelCase" -}
{-# LANGUAGE OverloadedStrings #-}

module Model.Engine.Murs where

import Data.Text (Text)
import Model.Engine.Types
import Model.Hitbox

---------------------------------------------------------------------------------
-- Fenêtrage des murs
--
-- Les murs du sujet peuvent être représentés par des listes infinies.
-- Pour éviter un ralentissement progressif, le moteur ne conserve qu'une
-- tranche utile autour de l'écran.
---------------------------------------------------------------------------------

margeMurNiveau :: Int
margeMurNiveau = margeHorsEcran

yMinMurNiveau, yMaxMurNiveau :: Int
yMinMurNiveau = -margeMurNiveau

yMaxMurNiveau = hauteurZoneJeu + margeMurNiveau

normaliserMur :: Hitbox -> Hitbox
normaliserMur (MurGauche pts) = MurGauche (normaliserPointsMur pts)
normaliserMur (MurDroit pts)  = MurDroit  (normaliserPointsMur pts)
normaliserMur h               = h

normaliserPointsMur :: [(Int, Int)] -> [(Int, Int)]
normaliserPointsMur =
  completerMurHaut yMaxMurNiveau
  . prendreJusquaHaut yMaxMurNiveau
  . supprimerBasMur yMinMurNiveau

-- | Supprime les segments définitivement passés sous la zone utile.
--   On garde le premier segment qui peut encore croiser yMin.
supprimerBasMur :: Int -> [(Int, Int)] -> [(Int, Int)]
supprimerBasMur yMin (p1 : p2@(_, y2) : rest)
  | y2 < yMin = supprimerBasMur yMin (p2 : rest)
  | otherwise = p1 : p2 : rest
supprimerBasMur _ pts = pts

-- | Coupe le mur dès qu'il dépasse suffisamment le haut de l'écran.
--   Sur une liste infinie, cette fonction produit une liste finie.
prendreJusquaHaut :: Int -> [(Int, Int)] -> [(Int, Int)]
prendreJusquaHaut _ [] = []
prendreJusquaHaut _ [p] = [p]
prendreJusquaHaut yMax (p1@(_, y1) : p2@(_, y2) : rest)
  | y1 > yMax = [p1, p2]
  | y2 > yMax = [p1, p2]
  | otherwise = p1 : prendreJusquaHaut yMax (p2 : rest)

-- | Complète le haut du mur après le défilement.
--   Cette reconstruction suffit pour les murs actuels :
--   mur vertical ou mur en dents de scie avec alternance de deux abscisses.
completerMurHaut :: Int -> [(Int, Int)] -> [(Int, Int)]
completerMurHaut yMax pts
  | length pts < 2 = pts
  | snd (last pts) >= yMax = pts
  | otherwise = completerMurHaut yMax (pts ++ [pointSuivantMur pts])

pointSuivantMur :: [(Int, Int)] -> (Int, Int)
pointSuivantMur pts =
  case reverse pts of
    (xDernier, yDernier) : (xAvant, yAvant) : _ ->
      let dy = max 1 (yDernier - yAvant)
          xSuivant =
            if xDernier == xAvant
              then xDernier
              else xAvant
      in (xSuivant, yDernier + dy)
    _ ->
      (0, yMaxMurNiveau)

defileMur :: Hitbox -> Hitbox
defileMur (MurGauche pts) =
  MurGauche (normaliserPointsMur (map descendrePointMur pts))
defileMur (MurDroit pts) =
  MurDroit (normaliserPointsMur (map descendrePointMur pts))
defileMur h = h

descendrePointMur :: (Int, Int) -> (Int, Int)
descendrePointMur (x, y) =
  (x, y - 1)

prop_pre_defileMurs :: MursNiveau -> Bool
prop_pre_defileMurs =
  prop_inv_mursNiveau

prop_post_defileMurs :: MursNiveau -> MursNiveau -> Bool
prop_post_defileMurs _ murs' =
     prop_inv_mursNiveau murs'
  && murNombrePoints (mnMurGauche murs') <= nombreMaxPointsMur
  && murNombrePoints (mnMurDroit murs')  <= nombreMaxPointsMur

murNombrePoints :: Hitbox -> Int
murNombrePoints (MurGauche pts) = length pts
murNombrePoints (MurDroit pts)  = length pts
murNombrePoints _               = 0

nombreMaxPointsMur :: Int
nombreMaxPointsMur = 80

mkMursNiveau :: Hitbox -> Hitbox -> Either Text MursNiveau
mkMursNiveau murG murD
  | not (estMurGauche murG)      = Left "mkMursNiveau: le mur gauche doit etre un MurGauche"
  | not (estMurDroit murD)       = Left "mkMursNiveau: le mur droit doit etre un MurDroit"
  | not (prop_inv_hitbox murG)   = Left "mkMursNiveau: mur gauche invalide"
  | not (prop_inv_hitbox murD)   = Left "mkMursNiveau: mur droit invalide"
  | not (prop_inv_hitbox murG')  = Left "mkMursNiveau: mur gauche normalise invalide"
  | not (prop_inv_hitbox murD')  = Left "mkMursNiveau: mur droit normalise invalide"
  | otherwise                    = Right (MursNiveau murG' murD')
  where
    murG' = normaliserMur murG
    murD' = normaliserMur murD

defileMurs :: MursNiveau -> MursNiveau
defileMurs murs =
  MursNiveau
    { mnMurGauche = defileMur (mnMurGauche murs)
    , mnMurDroit  = defileMur (mnMurDroit murs)
    }
