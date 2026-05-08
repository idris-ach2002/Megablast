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
        translate (-220) 250 $
          scale 0.42 0.42 $
            text "MEGABLAST"

    , color (greyN 0.78) $
        translate (-205) 205 $
          scale 0.14 0.14 $
            text "Choisissez le nombre de joueuses"
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
    translate (-245) (-315) $
      scale 0.105 0.105 $
        text "J1: fleches + entree   |   J2: z q s d + espace"
