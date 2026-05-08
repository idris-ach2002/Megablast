module View.AppView
  ( dessinerEtatApplication
  ) where

import Graphics.Gloss
import Controller.AppController
import Controller.Controller
import View.AccueilView
import View.Assets
import View.View

---------------------------------------------------------------------------------
-- Rendu de l'etat applicatif
---------------------------------------------------------------------------------

-- | Route le rendu vers la vue correspondant a l'etat applicatif courant.
--   Les vues specialisees restent dans le package View.
dessinerEtatApplication :: AssetsView -> EtatApplication AppStateFull -> Picture
dessinerEtatApplication assets etat =
  case etat of
    EcranAccueil ->
      dessinerAccueil

    Partie appState ->
      dessinerMoteurAvecAssets assets $ asMoteur $ asfBase appState
