module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
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
  -- Construction du moteur initial (on unwrap le Either ici, en IO)
  let moteurE = exempleMoteur
  case moteurE of
    Left err -> putStrLn $ "Erreur initialisation moteur: " ++ show err
    Right m  ->
      case mkCadence 3 of
        Left err  -> putStrLn $ "Erreur cadence tir: " ++ show err
        Right cadTir -> do
          assets <- chargerAssetsView
          let appState = mkAppStateFull (mkAppState m cadTir)
          play
            fenetre
            couleurFond
            fps
            appState
            (dessinerMoteurAvecAssets assets . asMoteur . asfBase)
            gererEvenementFull
            simulerStep