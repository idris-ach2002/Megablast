module View.AccueilView
  ( dessinerAccueil
  ) where

import Graphics.Gloss
import View.Background

---------------------------------------------------------------------------------
-- Ecran d'accueil
---------------------------------------------------------------------------------

dessinerAccueil :: Picture
dessinerAccueil =
  pictures
    [ dessinerFondEspace 0
    , dessinerTitre
    , dessinerBouton (makeColorI 45 95 145 235) (-85) "1" "Mode solo"
    , dessinerBouton (makeColorI 75 125 85 235) (-190) "2" "Mode duo local"
    , dessinerAideAccueil
    ]

dessinerTitre :: Picture
dessinerTitre =
  pictures
    [ color white $
        translate (-170) 200 $
          scale 0.5 0.5 $
            text "MEGABLAST"
    ]

dessinerBouton :: Color -> Float -> String -> String -> Picture
dessinerBouton couleur y touche libelle =
  pictures
    [ color couleur $
        translate 0 y $
          rectangleSolid 430 72

    , color white $
        translate 0 y $
          rectangleWire 430 72

    , color white $
        translate (-185) (y - 17) $
          scale 0.22 0.22 $
            text ("[" ++ touche ++ "]")

    , color white $
        translate (-95) (y - 16) $
          scale 0.18 0.18 $
            text libelle
    ]

dessinerAideAccueil :: Picture
dessinerAideAccueil =
  color (greyN 0.72) $
    translate (-250) (-315) $
      scale 0.15 0.15 $
        text "J1: fleches + entree   |   J2: z q s d + espace"
