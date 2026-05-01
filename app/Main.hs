module Main where

import Graphics.Gloss
import Model.Engine
import Model.Objects
import View.View
import Controller.Controller
import View.Assets

---------------------------------------------------------------------------------
-- Fenêtre Gloss
---------------------------------------------------------------------------------

fenetre :: Display
fenetre = InWindow "Xenon 2 : Megablast" (largeurFenetre, hauteurFenetre) (100, 100)

-- | Framerate cible (frames par seconde).
fps :: Int
fps = 60

---------------------------------------------------------------------------------
-- Point d'entrée
---------------------------------------------------------------------------------

main :: IO ()
main = do
  let config = configPartieDefaut
      moteurE = mkMoteurPartie config

  case moteurE of
    Left err ->
      putStrLn $ "Erreur initialisation moteur: " ++ show err

    Right m ->
      case mkCadence (cadenceTirJoueuse config) of
        Left err ->
          putStrLn $ "Erreur cadence tir: " ++ show err

        Right cadTir -> do
          assets <- chargerAssetsView

          let appState =
                mkAppStateFull (mkAppState m cadTir)

          play
            fenetre
            couleurFond
            fps
            appState
            (dessinerMoteurAvecAssets assets . asMoteur . asfBase)
            gererEvenementFull
            simulerStep