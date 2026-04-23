module View.VaisseauView where

import Graphics.Gloss
import Model.Hitbox
import Model.Objects
import Model.VaisseauForme
import qualified View.HitboxView as HB
import View.FormesView
import View.Theme

---------------------------------------------------------------------------------
-- Rendu spécialisé du vaisseau
---------------------------------------------------------------------------------

dessinerVaisseauJoueuse :: VaisseauJoueuse -> Picture
dessinerVaisseauJoueuse = dessinerPartiesVaisseau . vjForme

dessinerPartiesVaisseau :: PartiesVaisseau -> Picture
dessinerPartiesVaisseau pv =
  pictures
    [ dessinerCorps (pvCorps pv)
    , dessinerReacteur (pvReacteurG pv)
    , dessinerReacteur (pvReacteurD pv)
    , maybe blank dessinerCockpit (pvCockpit pv)
    ]

dessinerCorps :: Hitbox -> Picture
dessinerCorps (Model.Hitbox.Rectangle x y w h) =
  let bandeW = max 2 (w `div` 3)
      bandeX = x + (w - bandeW) `div` 2
      nezH = max 2 (h `div` 3)
      aileW = max 2 (w `div` 4)
      aileH = max 4 (h `div` 2)
  in pictures
      [ HB.dessinerRectangleSolid couleurVaisseauCorps x y w h
      , HB.dessinerRectangleWire couleurVaisseauBord x y w h
      , HB.dessinerRectangleSolid couleurVaisseauBande bandeX (y + 2) bandeW (max 2 (h - 4))
      , dessinerTriangleRectangleBasDroit couleurVaisseauCorps (x - aileW) (y + 1) aileW aileH
      , dessinerTriangleRectangleBasGauche couleurVaisseauCorps (x + w) (y + 1) aileW aileH
      , dessinerTriangleIsoceleHaut couleurVaisseauCorps (x + 1) (y + h - 1) (max 2 (w - 2)) nezH
      , dessinerLigneFermeeJeu couleurVaisseauBord (triangleIsoceleHautPoints (x + 1) (y + h - 1) (max 2 (w - 2)) nezH)
      ]
dessinerCorps h = HB.dessinerHitbox couleurVaisseauCorps h

dessinerReacteur :: Hitbox -> Picture
dessinerReacteur (Disque xc yc r) =
  let
      flammeCote = 10
  in pictures
      [  
        dessinerTriangleEquilateralHaut couleurFlamme (xc - flammeCote `div` 2) (yc - (r + flammeCote)) flammeCote
      , color couleurReacteur $ uncurry translate (HB.toGloss xc yc) $ circleSolid (fromIntegral r)
      , color couleurReacteurInterieur $ uncurry translate (HB.toGloss xc yc) $ circleSolid (fromIntegral r * 0.45)
      , color couleurVaisseauBord $ uncurry translate (HB.toGloss xc yc) $ thickCircle (fromIntegral r * 0.82) (max 1 (fromIntegral r * 0.16))
      ]
dessinerReacteur h = HB.dessinerHitbox couleurReacteur h

dessinerCockpit :: Hitbox -> Picture
dessinerCockpit (Disque xc yc r) =
  let (gx, gy) = HB.toGloss xc yc
      rf = fromIntegral r
  in pictures
      [ color couleurCockpit $ translate gx gy $ circleSolid rf
      , color couleurCockpitReflet $ translate (gx - rf * 0.35) (gy + rf * 0.25) $ circleSolid (rf * 0.35)
      , color couleurVaisseauBord $ translate gx gy $ thickCircle (rf * 0.82) (max 1 (rf * 0.15))
      ]
dessinerCockpit h = HB.dessinerHitbox couleurCockpit h
