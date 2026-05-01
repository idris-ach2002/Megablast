module View.HUDView
  ( dessinerHUDJoueuse
  , dessinerScore
  , dessinerTour
  ) where

import Graphics.Gloss
import Model.Engine
import Model.Objects
import qualified View.Theme as Theme


largeurFenetre, hauteurFenetre :: Int
largeurFenetre = 800
hauteurFenetre = 900

largeurBarreVie, hauteurBarreVie :: Int
largeurBarreVie = 150
hauteurBarreVie = 12

-- | Pour l’instant, le moteur garde des PV entiers simples.
--   Le HUD les convertit en pourcentage.
--   Si plus tard on ajoute un vrai pvMax dans VaisseauJoueuse, il suffira
--   de remplacer cette constante.
pvMaxJoueuseHUD :: Int
pvMaxJoueuseHUD = 100

dessinerHUDJoueuse :: Int -> Int -> VaisseauJoueuse -> Picture
dessinerHUDJoueuse tour idx v =
  pictures
    [ dessinerNomJoueuse idx xBase yBase
    , dessinerBarreVie tour (xBase + 45) (yBase - 2) (pourcentageVie v)
    , dessinerEssais (xBase + 210) yBase v
    ]
  where
    xBase =
      fromIntegral (- largeurFenetre `div` 2) + 10

    yBase =
      fromIntegral (hauteurFenetre `div` 2) - 24 - fromIntegral (idx * 30)

dessinerNomJoueuse :: Int -> Float -> Float -> Picture
dessinerNomJoueuse idx x y =
  color white $
    translate x y $
      scale 0.12 0.12 $
        text ("J" ++ show (idx + 1))

dessinerEssais :: Float -> Float -> VaisseauJoueuse -> Picture
dessinerEssais x y v =
  color (greyN 0.85) $
    translate x y $
      scale 0.10 0.10 $
        text ("Essais: " ++ show (vjEssais v))

pourcentageVie :: VaisseauJoueuse -> Int
pourcentageVie v =
  clamp 0 100 $
    vjPv v * 100 `div` pvMaxJoueuseHUD

clamp :: Int -> Int -> Int -> Int
clamp mini maxi x =
  max mini (min maxi x)

---------------------------------------------------------------------------------
-- Barre de vie
---------------------------------------------------------------------------------

dessinerBarreVie :: Int -> Float -> Float -> Int -> Picture
dessinerBarreVie tour x y pv =
  pictures
    [ dessinerFondBarre x y
    , dessinerRemplissageBarre x y pv
    , dessinerRefletBarre tour x y pv
    , dessinerContourBarre x y
    , dessinerTexteVie x y pv
    ]

dessinerFondBarre :: Float -> Float -> Picture
dessinerFondBarre x y =
  color Theme.couleurVieFond $
    translate (x + demiLargeur) y $
      rectangleSolid largeur hauteur
  where
    largeur = fromIntegral largeurBarreVie
    hauteur = fromIntegral hauteurBarreVie
    demiLargeur = largeur / 2

dessinerRemplissageBarre :: Float -> Float -> Int -> Picture
dessinerRemplissageBarre x y pv =
  color (couleurVie pv) $
    translate (x + largeurRemplie / 2) y $
      rectangleSolid largeurRemplie hauteur
  where
    largeurRemplie =
      fromIntegral (largeurBarreVie * pv `div` 100)

    hauteur =
      fromIntegral hauteurBarreVie

couleurVie :: Int -> Color
couleurVie pv
  | pv <= 25  = Theme.couleurVieFaible
  | pv <= 60  = Theme.couleurVieMoyenne
  | otherwise = Theme.couleurVieHaute

dessinerContourBarre :: Float -> Float -> Picture
dessinerContourBarre x y =
  color Theme.couleurVieContour $
    translate (x + largeur / 2) y $
      rectangleWire largeur hauteur
  where
    largeur = fromIntegral largeurBarreVie
    hauteur = fromIntegral hauteurBarreVie

-- | Petit reflet qui se déplace dans la partie remplie.
--   C’est volontairement simple : pas besoin d’état supplémentaire.
dessinerRefletBarre :: Int -> Float -> Float -> Int -> Picture
dessinerRefletBarre tour x y pv
  | pv <= 0 = blank
  | otherwise =
      color Theme.couleurVieReflet $
        translate (x + fromIntegral position) y $
          rectangleSolid 10 (fromIntegral hauteurBarreVie)
  where
    largeurRemplie =
      max 1 (largeurBarreVie * pv `div` 100)

    position =
      tour `mod` largeurRemplie

dessinerTexteVie :: Float -> Float -> Int -> Picture
dessinerTexteVie x y pv =
  color white $
    translate (x + fromIntegral largeurBarreVie + 8) (y - 4) $
      scale 0.08 0.08 $
        text (show pv ++ "%")

---------------------------------------------------------------------------------
-- Score et tour
---------------------------------------------------------------------------------

dessinerScore :: Score -> Picture
dessinerScore score =
  let x = fromIntegral (- largeurFenetre `div` 2) + 10
      y = fromIntegral (- hauteurFenetre `div` 2) + 8
      txt = "Score: " ++ show (scoreValeur score)
  in color (greyN 0.85) $
       translate x y $
         scale 0.12 0.12 $
           text txt

dessinerTour :: Int -> Picture
dessinerTour t =
  let x = fromIntegral (largeurFenetre `div` 2) - 120
      y = fromIntegral (- hauteurFenetre `div` 2) + 8
  in color (greyN 0.6) $
       translate x y $
         scale 0.10 0.10 $
           text ("Tour: " ++ show t)