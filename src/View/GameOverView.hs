module View.GameOverView
  ( dessinerGameOver
  ) where

import Graphics.Gloss
import View.Background

---------------------------------------------------------------------------------
-- Ecran de game over
---------------------------------------------------------------------------------

dessinerGameOver :: Picture
dessinerGameOver =
  pictures
    [ dessinerFondEspace 0
    , dessinerTitreGameOver
    , dessinerBoutonRetourAccueil
    ]

dessinerTitreGameOver :: Picture
dessinerTitreGameOver =
  pictures
    [ color red $
        translate (-180) 90 $
          scale 0.42 0.42 $
            text "GAME OVER"
    ]

dessinerBoutonRetourAccueil :: Picture
dessinerBoutonRetourAccueil =
  pictures
    [ color (makeColorI 45 95 145 235) $
        translate 0 (-70) $
          rectangleSolid 430 72

    , color white $
        translate 0 (-70) $
          rectangleWire 430 72

    , color white $
        translate (-185) (-87) $
          scale 0.20 0.20 $
            text "[Entree]"

    ]
