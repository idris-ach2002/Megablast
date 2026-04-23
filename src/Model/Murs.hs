{- HLINT ignore "Use camelCase" -}

module Model.Murs where

import Model.Hitbox

-- | Symétrie horizontale d'une liste de points par rapport à la droite x = largeur / 2.
--   Un point (x,y) devient (largeur - x, y).
symetrie_points :: Int -> [(Int, Int)] -> [(Int, Int)]
symetrie_points largeur = fmap (\(x, y) -> (largeur - x, y))

-- | Construit un mur droit comme symétrique d'un mur gauche.
mur_droit_symetrique :: Int -> Hitbox -> Hitbox
mur_droit_symetrique largeur (MurGauche pts) = MurDroit (symetrie_points largeur pts)
mur_droit_symetrique _ h = h

mur_gauche_dents_scie :: Int -> Int -> Hitbox
mur_gauche_dents_scie dx dy = MurGauche ((0, 0) : aux 0 0)
  where
    aux prevX prevY =
      let (newX, newY) =
            if prevX == 0
              then (dx, prevY + dy)
              else (0,  prevY + dy)
      in (newX, newY) : aux newX newY

mur_droit_dents_scie :: Int -> Int -> Int -> Hitbox
mur_droit_dents_scie largeur dx dy =
  mur_droit_symetrique largeur (mur_gauche_dents_scie dx dy)

mur_gauche_vertical :: Int -> Int -> Hitbox
mur_gauche_vertical x dy = MurGauche (aux 0)
  where
    aux y = (x, y) : aux (y + dy)

mur_droit_vertical :: Int -> Int -> Int -> Hitbox
mur_droit_vertical largeur x dy =
  mur_droit_symetrique largeur (mur_gauche_vertical x dy)

-- Question 2.1:
mur_dent_scie_Exam :: Hitbox
mur_dent_scie_Exam = mur_gauche_dents_scie 5 10

-- Question 2.2:
mur_gauche_vertical_10 :: Hitbox
mur_gauche_vertical_10 = mur_gauche_vertical 10 10
