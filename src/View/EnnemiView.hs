module View.EnnemiView
  ( dessinerEnnemi
  ) where

import Graphics.Gloss
import Model.Hitbox
import Model.Objects
import qualified View.HitboxView as HB
import qualified View.Theme as Theme
import View.FormesView

---------------------------------------------------------------------------------
-- Rendu spécialisé des ennemis
---------------------------------------------------------------------------------

dessinerEnnemi :: Ennemi -> Picture
dessinerEnnemi e =
  case eHitbox e of
    Model.Hitbox.Rectangle x y w h -> dessinerVaisseauEnnemi x y w h
    h                 -> HB.dessinerHitbox Theme.couleurEnnemiCorps h

dessinerVaisseauEnnemi :: Int -> Int -> Int -> Int -> Picture
dessinerVaisseauEnnemi x y w h =
  pictures
    [ dessinerAileGauche x y w h
    , dessinerAileDroite x y w h
    , dessinerCorpsEnnemi x y w h
    , dessinerNezEnnemi x y w h
    , dessinerCockpitEnnemi x y w h
    , dessinerReacteursEnnemi x y w h
    ]

---------------------------------------------------------------------------------
-- Parties du vaisseau ennemi
---------------------------------------------------------------------------------

dessinerCorpsEnnemi :: Int -> Int -> Int -> Int -> Picture
dessinerCorpsEnnemi x y w h =
  pictures
    [ HB.dessinerRectangleSolid Theme.couleurEnnemiCorps corpsX corpsY corpsW corpsH
    , HB.dessinerRectangleWire  Theme.couleurEnnemiBord  corpsX corpsY corpsW corpsH
    , HB.dessinerRectangleSolid Theme.couleurEnnemiBande bandeX corpsY bandeW corpsH
    ]
  where
    corpsW = max 8 (w * 3 `div` 5)
    corpsH = max 8 (h * 2 `div` 3)
    corpsX = x + (w - corpsW) `div` 2
    corpsY = y + h `div` 4

    bandeW = max 2 (corpsW `div` 3)
    bandeX = corpsX + (corpsW - bandeW) `div` 2

dessinerNezEnnemi :: Int -> Int -> Int -> Int -> Picture
dessinerNezEnnemi x y w h =
  dessinerTriangleIsoceleBas
    Theme.couleurEnnemiCorps
    nezX
    y
    nezW
    nezH
  where
    nezW = max 8 (w * 3 `div` 5)
    nezH = max 5 (h `div` 3)
    nezX = x + (w - nezW) `div` 2

dessinerAileGauche :: Int -> Int -> Int -> Int -> Picture
dessinerAileGauche x y w h =
  dessinerTriangleJeu
    Theme.couleurEnnemiAile
    (corpsX, y + h `div` 3)
    (x,      y + h `div` 2)
    (corpsX, y + h - 2)
  where
    corpsW = max 8 (w * 3 `div` 5)
    corpsX = x + (w - corpsW) `div` 2

dessinerAileDroite :: Int -> Int -> Int -> Int -> Picture
dessinerAileDroite x y w h =
  dessinerTriangleJeu
    Theme.couleurEnnemiAile
    (corpsDroit, y + h `div` 3)
    (x + w,      y + h `div` 2)
    (corpsDroit, y + h - 2)
  where
    corpsW = max 8 (w * 3 `div` 5)
    corpsX = x + (w - corpsW) `div` 2
    corpsDroit = corpsX + corpsW

dessinerCockpitEnnemi :: Int -> Int -> Int -> Int -> Picture
dessinerCockpitEnnemi x y w h =
  cercleJeu
    Theme.couleurEnnemiCockpit
    (x + w `div` 2)
    (y + h `div` 2)
    (max 2 (min w h `div` 5))

dessinerReacteursEnnemi :: Int -> Int -> Int -> Int -> Picture
dessinerReacteursEnnemi x y w h =
  pictures
    [ cercleJeu Theme.couleurEnnemiReacteur (x + w `div` 3)       (y + h - 2) rayon
    , cercleJeu Theme.couleurEnnemiReacteur (x + 2 * w `div` 3)   (y + h - 2) rayon
    , cercleJeu Theme.couleurEnnemiFlamme   (x + w `div` 3)       (y + h + 3) petitRayon
    , cercleJeu Theme.couleurEnnemiFlamme   (x + 2 * w `div` 3)   (y + h + 3) petitRayon
    ]
  where
    rayon = max 2 (min w h `div` 8)
    petitRayon = max 1 (rayon - 1)

---------------------------------------------------------------------------------
-- Petite primitive locale
---------------------------------------------------------------------------------

cercleJeu :: Color -> Int -> Int -> Int -> Picture
cercleJeu c x y r =
  let (gx, gy) = HB.toGloss x y
  in color c $
       translate gx gy $
         circleSolid (fromIntegral r)