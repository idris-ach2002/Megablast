module View.GameOverView
  ( dessinerGameOver
  ) where

import Data.List (intercalate)
import Graphics.Gloss
import Model.Score (Score, scoreValeur)
import View.Background

---------------------------------------------------------------------------------
-- Ecran de game over
---------------------------------------------------------------------------------

-- | Affiche l'ecran de fin avec un resume du score.
dessinerGameOver :: [Score] -> Picture
dessinerGameOver scores =
  pictures
    [ dessinerFondEspace 0
    , dessinerTitreGameOver
    , dessinerResumeScore scores
    , dessinerBoutonRetourAccueil
    ]

dessinerTitreGameOver :: Picture
dessinerTitreGameOver =
  pictures
    [ color red $
        translate (-180) 135 $
          scale 0.42 0.42 $
            text "GAME OVER"
    ]

dessinerResumeScore :: [Score] -> Picture
dessinerResumeScore [] =
  dessinerTexteCentre 45 0.20 "Score final: 0"

dessinerResumeScore [score] =
  dessinerTexteCentre 45 0.20 $
    "Score final: " ++ show (scoreValeur score)

dessinerResumeScore scores =
  pictures
    [ dessinerTexteCentre 55 0.18 titreGagnant
    , dessinerTexteCentre 25 0.15 $ "Score gagnant: " ++ show scoreGagnant
    , dessinerScoresFinPartie (-20) scores
    ]
  where
    gagnantes = indicesGagnantes scores

    scoreGagnant =
      maximum (map scoreValeur scores)

    titreGagnant =
      case gagnantes of
        [indice] ->
          "Gagnant: J" ++ show (indice + 1)

        _ ->
          "Egalite: " ++ intercalate " / " (map nomJoueuse gagnantes)

dessinerScoresFinPartie :: Float -> [Score] -> Picture
dessinerScoresFinPartie yDepart scores =
  pictures $
    zipWith (dessinerLigneScore yDepart) [0 ..] scores

dessinerLigneScore :: Float -> Int -> Score -> Picture
dessinerLigneScore yDepart indice score =
  dessinerTexteCentre y 0.12 $
    nomJoueuse indice ++ ": " ++ show (scoreValeur score)
  where
    y =
      yDepart - fromIntegral indice * 22

indicesGagnantes :: [Score] -> [Int]
indicesGagnantes [] =
  []

indicesGagnantes scores =
  [ indice
  | (indice, score) <- zip [0 ..] scores
  , scoreValeur score == meilleurScore
  ]
  where
    meilleurScore =
      maximum (map scoreValeur scores)

nomJoueuse :: Int -> String
nomJoueuse indice =
  "J" ++ show (indice + 1)

dessinerTexteCentre :: Float -> Float -> String -> Picture
dessinerTexteCentre y facteur contenu =
  color white $
    translate x y $
      scale facteur facteur $
        text contenu
  where
    -- Approximation simple pour centrer les textes Gloss.
    x =
      - fromIntegral (length contenu) * 52 * facteur / 2

dessinerBoutonRetourAccueil :: Picture
dessinerBoutonRetourAccueil =
  pictures
    [ color (makeColorI 45 95 145 235) $
        translate 0 (-150) $
          rectangleSolid 430 72

    , color white $
        translate 0 (-150) $
          rectangleWire 430 72

    , color white $
        translate (-185) (-167) $
          scale 0.20 0.20 $
            text "Ecran d'accueil [Entree]"

    ]
