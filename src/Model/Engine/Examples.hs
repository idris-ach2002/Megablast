{-# LANGUAGE OverloadedStrings #-}

module Model.Engine.Examples where

import Data.Text (Text)
import Model.Engine.Murs
import Model.Engine.Types
import Model.Hitbox
import Model.Meteore
import Model.Murs
import Model.Objects
import Model.VaisseauForme

exempleMoteur :: Either Text Moteur
exempleMoteur = do
  cad <- mkCadence 1
  formeVaisseau1 <- mkPartiesVaisseauStandard 50 20
  formeVaisseau2 <- mkPartiesVaisseauStandard 90 20
  cadV <- mkCadence 1
  vaisseau <- mkVaisseauJoueuse formeVaisseau1 1 2 cadV
  vaisseau2 <- mkVaisseauJoueuse formeVaisseau2 1 2 cadV
  hObs <- mkRectangle 30 300 40 20
  obs <- mkObstacle hObs
  let oracle = Scripted [Attendre, Deplacer Gauche, Tirer] 0
      murGauche = mur_gauche_dents_scie 50 60
      murDroit  = mur_droit_dents_scie largeurZoneJeu 50 60
  murs <- mkMursNiveau murGauche murDroit
  pv <- mkPV 3
  cadEnn <- mkCadence 2
  hEnn <- mkRectangle 60 500 12 12
  enn <- mkEnnemi hEnn pv oracle cadEnn
  cadMeteoreRapide <- mkCadence 1
  cadMeteoreMoyen  <- mkCadence 2
  cadMeteoreLent   <- mkCadence 3

  meteore1 <- mkMeteoreRond 180 880 14 cadMeteoreMoyen
  meteore2 <- mkMeteoreRond 360 920 18 cadMeteoreLent
  meteore3 <- mkMeteoreRond 560 900 12 cadMeteoreRapide
  meteore4 <- mkMeteoreRond 260 940 16 cadMeteoreMoyen
  meteore5 <- mkMeteoreRond 480 880 20 cadMeteoreLent


  mkMoteur
    [obs]
    []
    [enn]
    [vaisseau, vaisseau2]
    murs
    cad
    [ EvenementPlanifie 5   (AppObstacle obs)
    , EvenementPlanifie 8   (AppMeteore meteore1)
    , EvenementPlanifie 90  (AppMeteore meteore2)
    , EvenementPlanifie 160 (AppMeteore meteore3)
    , EvenementPlanifie 230 (AppMeteore meteore4)
    , EvenementPlanifie 320 (AppMeteore meteore5)
    , EvenementPlanifie 10  (DisparEnnemi 0)
    ]
    0
    42
