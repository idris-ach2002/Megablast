module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Engine
import Objects
import View
import Controller

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
          let appState = mkAppStateFull (mkAppState m cadTir)
          play
            fenetre
            couleurFond           -- couleur de fond (écrasée par dessinerMoteur)
            fps
            appState
            (dessinerMoteur . asMoteur . asfBase)   -- View
            gererEvenementFull                       -- Controller (events)
            simulerStep                              -- Controller (step)