{- HLINT ignore "Use camelCase" -}

module View.View where

import Graphics.Gloss
import Model.Hitbox
import Model.Objects
import Model.Engine

---------------------------------------------------------------------------------
-- Constantes visuelles
---------------------------------------------------------------------------------

-- | Couleurs utilisées pour chaque type d'objet.
couleurVaisseau, couleurEnnemi, couleurObstacle,
  couleurProjJoueuse, couleurProjEnnemi,
  couleurMurGauche, couleurMurDroit,
  couleurFond :: Color
couleurVaisseau    = makeColorI 100 200 255 255   -- bleu clair
couleurEnnemi      = makeColorI 255  80  80 255   -- rouge
couleurObstacle    = makeColorI 160 160 160 255   -- gris
couleurProjJoueuse = makeColorI 255 255  50 255   -- jaune
couleurProjEnnemi  = makeColorI 255 100   0 255   -- orange
couleurMurGauche   = makeColorI  80 200  80 255   -- vert
couleurMurDroit    = makeColorI  80 200  80 255
couleurFond        = makeColorI  10  10  25 255   -- quasi-noir

-- | Rayon affiché quand une Hitbox est un simple Point.
rayonPoint :: Float
rayonPoint = 3

---------------------------------------------------------------------------------
-- Conversion de coordonnées
--
-- Gloss centre son repère en (0,0) au milieu de la fenêtre.
-- Notre jeu a (0,0) en bas-à-gauche.  On translate donc :
--   x_gloss = x_jeu - largeurFenetre/2
--   y_gloss = y_jeu - hauteurFenetre/2
---------------------------------------------------------------------------------

-- | Dimensions de la fenêtre (en pixels).  Modifiez ici pour changer la taille.
largeurFenetre, hauteurFenetre :: Int
largeurFenetre = 800
hauteurFenetre = 900

-- | Convertit un point jeu → coordonnées Gloss (Float).
toGloss :: Int -> Int -> (Float, Float)
toGloss x y =
  ( fromIntegral x - fromIntegral largeurFenetre  / 2
  , fromIntegral y - fromIntegral hauteurFenetre / 2
  )

---------------------------------------------------------------------------------
-- Rendu d'une Hitbox
---------------------------------------------------------------------------------

-- | Dessine une Hitbox avec la couleur donnée (filled).
dessinerHitbox :: Color -> Hitbox -> Picture
dessinerHitbox c (Point x y) =
  let (gx, gy) = toGloss x y
  in color c $ translate gx gy $ circleSolid rayonPoint

dessinerHitbox c (Disque xc yc r) =
  let (gx, gy) = toGloss xc yc
  in color c $ translate gx gy $ circleSolid (fromIntegral r)

dessinerHitbox c (Model.Hitbox.Rectangle x y w h) =
  let (gx, gy) = toGloss x y
      fw = fromIntegral w
      fh = fromIntegral h
  -- Gloss dessine les rectangles centrés, on translate d'un demi-côté.
  in color c $ translate (gx + fw / 2) (gy + fh / 2) $ rectangleSolid fw fh

dessinerHitbox c (Composee hs) =
  pictures $ map (dessinerHitbox c) hs

-- | Mur gauche : on dessine chaque segment du brisé.
dessinerHitbox c (MurGauche pts) =
  pictures $ zipWith dessinerSegmentGauche pts (tail pts)
  where
    -- Pour chaque paire de points consécutifs, on trace un polygone
    -- qui couvre la zone "à gauche" du segment jusqu'au bord gauche de l'écran.
    dessinerSegmentGauche (x1, y1) (x2, y2) =
      let (gx1, gy1) = toGloss x1 y1
          (gx2, gy2) = toGloss x2 y2
          bord       = fromIntegral (- largeurFenetre `div` 2) -- bord gauche Gloss
          poly       = [ (bord, gy1), (gx1, gy1)
                       , (gx2, gy2), (bord, gy2) ]
      in color c $ polygon poly

dessinerHitbox c (MurDroit pts) =
  pictures $ zipWith dessinerSegmentDroit pts (tail pts)
  where
    dessinerSegmentDroit (x1, y1) (x2, y2) =
      let (gx1, gy1) = toGloss x1 y1
          (gx2, gy2) = toGloss x2 y2
          bord       = fromIntegral (largeurFenetre `div` 2)
          poly       = [ (gx1, gy1), (bord, gy1)
                       , (bord, gy2), (gx2, gy2) ]
      in color c $ polygon poly

---------------------------------------------------------------------------------
-- Rendu de chaque entité du jeu
---------------------------------------------------------------------------------

dessinerObstacle :: Obstacle -> Picture
dessinerObstacle (Obstacle h) = dessinerHitbox couleurObstacle h

dessinerProjectile :: Projectile -> Picture
dessinerProjectile p =
  let c = case prOwner p of
            TirJoueuse -> couleurProjJoueuse
            TirEnnemi  -> couleurProjEnnemi
  in dessinerHitbox c (prHitbox p)

dessinerEnnemi :: Ennemi -> Picture
dessinerEnnemi e = dessinerHitbox couleurEnnemi (eHitbox e)

dessinerVaisseau :: VaisseauJoueuse -> Picture
dessinerVaisseau v = dessinerHitbox couleurVaisseau (vjHitbox v)

---------------------------------------------------------------------------------
-- HUD (Heads-Up Display)
---------------------------------------------------------------------------------

-- | Affiche la barre de vie et les essais pour la joueuse d'indice i.
dessinerHUD :: Int -> VaisseauJoueuse -> Picture
dessinerHUD idx v =
  let xBase  = fromIntegral (- largeurFenetre `div` 2) + 10
      yBase  = fromIntegral (hauteurFenetre `div` 2) - 20 - fromIntegral (idx * 25)
      pvTxt  = "PV: " ++ show (vjPv v)
      essTxt = "  Essais: " ++ show (vjEssais v)
      txt    = "J" ++ show (idx + 1) ++ "  " ++ pvTxt ++ essTxt
  in color white $ translate xBase yBase $ scale 0.13 0.13 $ text txt

-- | Affiche le numéro de tour en bas à droite.
dessinerTour :: Int -> Picture
dessinerTour t =
  let x = fromIntegral (largeurFenetre `div` 2) - 120
      y = fromIntegral (- hauteurFenetre `div` 2) + 8
  in color (greyN 0.6) $ translate x y $ scale 0.10 0.10 $ text ("Tour: " ++ show t)

-- | Écran de Game Over.
dessinerGameOver :: Picture
dessinerGameOver =
  pictures
    [ color (makeColorI 0 0 0 180) $ rectangleSolid (fromIntegral largeurFenetre)
                                                     (fromIntegral hauteurFenetre)
    , color red  $ translate (-140) 20   $ scale 0.4 0.4 $ text "GAME OVER"
    , color white$ translate (-110) (-30)$ scale 0.15 0.15$ text "Appuyez sur R pour recommencer"
    ]

---------------------------------------------------------------------------------
-- Rendu principal du moteur
---------------------------------------------------------------------------------

-- | Transforme l'état complet du moteur en une Picture Gloss.
dessinerMoteur :: Moteur -> Picture
dessinerMoteur m
  | not (prop_partie_en_cours m) = dessinerGameOver
  | otherwise =
      pictures $
        [ color couleurFond $ rectangleSolid fw fh ]   -- fond
        ++ map dessinerObstacle   (mObstacles   m)
        ++ map dessinerProjectile (mProjectiles m)
        ++ map dessinerEnnemi     (mEnnemis     m)
        ++ map dessinerVaisseau   (mJoueuses    m)
        ++ zipWith dessinerHUD [0..] (mJoueuses m)
        ++ [ dessinerTour (mTour m) ]
  where
    fw = fromIntegral largeurFenetre
    fh = fromIntegral hauteurFenetre
