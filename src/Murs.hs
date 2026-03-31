{- HLINT ignore "Use camelCase" -}

module Murs where

import Hitbox

mur_gauche_dents_scie :: Int -> Int -> Hitbox
mur_gauche_dents_scie dx dy = MurGauche $ (0,0) : aux 0 0 where
    aux prevX prevY = 
      let 
        (newX, newY) = 
          if prevX == 0 then (dx, prevY + dy)
          else (0, prevY + dy)
      in
      (newX, newY) : aux newX newY

mur_gauche_vertical :: Int -> Int -> Hitbox
mur_gauche_vertical x dy = MurGauche (aux 0)
  where
    aux y = (x, y) : aux (y + dy)

-- Question 2.1:
mur_dent_scie_Exam :: Hitbox
mur_dent_scie_Exam = mur_gauche_dents_scie 5 10

-- Question 2.2:
mur_gauche_vertical_10 :: Hitbox
mur_gauche_vertical_10 = mur_gauche_vertical 10 10