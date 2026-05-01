{-# LANGUAGE OverloadedStrings #-}

module Model.Engine.GameConfig where

import Data.Text (Text)
import Model.Engine.Murs
import Model.Engine.Types
import Model.Murs
import Model.Objects
import Model.VaisseauForme

---------------------------------------------------------------------------------
-- Configuration de partie
---------------------------------------------------------------------------------

data ModePartie
  = MonoJoueur
  | DuoJoueurs
  deriving (Eq, Show)

data ConfigPartie = ConfigPartie
  { cfgMode :: ModePartie
  , cfgSeed :: Int
  } deriving (Eq, Show)

configPartieDefaut :: ConfigPartie
configPartieDefaut =
  ConfigPartie
    { cfgMode = MonoJoueur
    , cfgSeed = 42
    }

configPartieDuo :: ConfigPartie
configPartieDuo =
  configPartieDefaut { cfgMode = DuoJoueurs }

prop_inv_configPartie :: ConfigPartie -> Bool
prop_inv_configPartie _ =
  True

---------------------------------------------------------------------------------
-- Paramètres initiaux
---------------------------------------------------------------------------------

pvInitialJoueuse :: Int
pvInitialJoueuse =
  100

essaisInitiauxJoueuse :: Int
essaisInitiauxJoueuse =
  2

cadenceScrollInitiale :: Int
cadenceScrollInitiale =
  1

cadenceJoueuseInitiale :: Int
cadenceJoueuseInitiale =
  1

positionJoueuseSolo :: (Int, Int)
positionJoueuseSolo =
  (388, 30)

positionsJoueusesDuo :: [(Int, Int)]
positionsJoueusesDuo =
  [ (340, 30)
  , (430, 30)
  ]

---------------------------------------------------------------------------------
-- Construction du moteur initial
---------------------------------------------------------------------------------

mkMoteurPartie :: ConfigPartie -> Either Text Moteur
mkMoteurPartie config = do
  cadScroll <- mkCadence cadenceScrollInitiale
  cadJoueuse <- mkCadence cadenceJoueuseInitiale

  joueuses <- mkJoueusesInitiales (cfgMode config) cadJoueuse
  murs <- mkMursPartie

  mkMoteur
    []                 -- obstacles actifs
    []                 -- projectiles actifs
    []                 -- ennemis actifs
    joueuses
    murs
    cadScroll
    []                 -- script initial vide : les objets seront générés dynamiquement
    0
    (cfgSeed config)

mkJoueusesInitiales :: ModePartie -> Cadence -> Either Text [VaisseauJoueuse]
mkJoueusesInitiales mode cad =
  case mode of
    MonoJoueur ->
      mapM (`mkJoueuseInitiale` cad) [positionJoueuseSolo]

    DuoJoueurs ->
      mapM (`mkJoueuseInitiale` cad) positionsJoueusesDuo

mkJoueuseInitiale :: (Int, Int) -> Cadence -> Either Text VaisseauJoueuse
mkJoueuseInitiale (x, y) cad = do
  forme <- mkPartiesVaisseauStandard x y
  mkVaisseauJoueuse forme pvInitialJoueuse essaisInitiauxJoueuse cad

mkMursPartie :: Either Text MursNiveau
mkMursPartie =
  mkMursNiveau murGauche murDroit
  where
    murGauche =
      mur_gauche_dents_scie 50 60

    murDroit =
      mur_droit_dents_scie largeurZoneJeu 50 60