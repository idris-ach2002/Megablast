{-# HLINT ignore "Use camelCase" #-}

module Controller.AppController
  ( EtatApplication(..)
  , etatApplicationInitial
  , lancerPartie
  , gererEvenementEtatApplication
  , simulerEtatApplication
  , prop_inv_etatApplication
  , prop_inv_etatApplicationFull
  ) where

import Graphics.Gloss.Interface.Pure.Game
import Controller.Controller
import Model.Engine
import Model.Objects

---------------------------------------------------------------------------------
-- Etat applicatif de haut niveau
---------------------------------------------------------------------------------

-- | Etat de haut niveau de l'application Gloss.
--
--   Le moteur reste responsable de l'etat de jeu pur. Cet etat applicatif
--   separe explicitement l'ecran de navigation de la partie en cours.
data EtatApplication a
  = EcranAccueil
  | Partie a

etatApplicationInitial :: EtatApplication AppStateFull
etatApplicationInitial =
  EcranAccueil

-- | Invariant generique de l'etat applicatif.
--
--   L'ecran d'accueil ne contient pas d'etat de jeu.
--   Une partie est valide lorsque le predicat fourni par l'appelant l'est.
prop_inv_etatApplication :: (a -> Bool) -> EtatApplication a -> Bool
prop_inv_etatApplication _ EcranAccueil =
  True
prop_inv_etatApplication propPartie (Partie partie) =
  propPartie partie

prop_inv_etatApplicationFull :: EtatApplication AppStateFull -> Bool
prop_inv_etatApplicationFull =
  prop_inv_etatApplication prop_pre_simulerStep

---------------------------------------------------------------------------------
-- Construction d'une partie
---------------------------------------------------------------------------------

-- | Construit l'etat de controleur correspondant a une configuration de partie.
lancerPartie :: ConfigPartie -> EtatApplication AppStateFull
lancerPartie config =
  case mkMoteurPartie config of
    Left _ ->
      EcranAccueil

    Right moteur ->
      case mkCadence (cadenceTirJoueuse config) of
        Left _ ->
          EcranAccueil

        Right cadTir ->
          Partie $ mkAppStateFull $ mkAppState moteur cadTir

---------------------------------------------------------------------------------
-- Gestion des evenements Gloss
---------------------------------------------------------------------------------

gererEvenementEtatApplication
  :: Event
  -> EtatApplication AppStateFull
  -> EtatApplication AppStateFull
gererEvenementEtatApplication event etat =
  case etat of
    EcranAccueil ->
      gererEvenementAccueil event

    Partie appState ->
      gererEvenementPartie event appState

gererEvenementAccueil :: Event -> EtatApplication AppStateFull
gererEvenementAccueil event =
  case event of
    EventKey (Char '1') Down _ _ ->
      lancerPartie configPartieDefaut

    EventKey (Char '2') Down _ _ ->
      lancerPartie configPartieDuo

    _ ->
      EcranAccueil

gererEvenementPartie :: Event -> AppStateFull -> EtatApplication AppStateFull
gererEvenementPartie event appState =
  case event of
    EventKey (SpecialKey KeyEsc) Down _ _ ->
      EcranAccueil

    _ ->
      Partie $ gererEvenementFull event appState

---------------------------------------------------------------------------------
-- Simulation
---------------------------------------------------------------------------------

simulerEtatApplication
  :: Float
  -> EtatApplication AppStateFull
  -> EtatApplication AppStateFull
simulerEtatApplication dt etat =
  case etat of
    EcranAccueil ->
      EcranAccueil

    Partie appState ->
      Partie $ simulerStep dt appState
