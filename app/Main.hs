module Main where

import Graphics.Gloss
import Controller.AppController
import View.AppView
import View.Assets
import View.View

---------------------------------------------------------------------------------
-- Fenetre Gloss
---------------------------------------------------------------------------------

fenetre :: Display
fenetre = InWindow "Xenon 2 : Megablast" (largeurFenetre, hauteurFenetre) (100, 100)

-- | Framerate cible (frames par seconde).
fps :: Int
fps = 60

---------------------------------------------------------------------------------
-- Point d'entree
---------------------------------------------------------------------------------

main :: IO ()
main = do
  assets <- chargerAssetsView

  play
    fenetre
    couleurFond
    fps
    etatApplicationInitial
    (dessinerEtatApplication assets)
    gererEvenementEtatApplication
    simulerEtatApplication
