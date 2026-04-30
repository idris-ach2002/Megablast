{-# LANGUAGE OverloadedStrings #-}

module Model.Engine.EnnemiSpawn where

import Model.Engine.Types
import Model.Hitbox
import Model.Objects
import System.Random (randomR)

---------------------------------------------------------------------------------
-- Apparition automatique des ennemis
---------------------------------------------------------------------------------

periodeApparitionEnnemi :: Int
periodeApparitionEnnemi = 240

maxEnnemisActifs :: Int
maxEnnemisActifs = 3

largeurEnnemiSpawn, hauteurEnnemiSpawn :: Int
largeurEnnemiSpawn = 25
hauteurEnnemiSpawn = 30

yApparitionEnnemi :: Int
yApparitionEnnemi = hauteurZoneJeu - 80

xMinApparitionEnnemi, xMaxApparitionEnnemi :: Int
xMinApparitionEnnemi = 70
xMaxApparitionEnnemi = largeurZoneJeu - 150

pvEnnemiSpawn :: Int
pvEnnemiSpawn = 2

cadenceTirEnnemiSpawn :: Int
cadenceTirEnnemiSpawn = 200

-- | Ajoute éventuellement un ennemi au moteur.
--   La décision est simple :
--   - pas au tour 0 ;
--   - un essai d'apparition toutes les periodeApparitionEnnemi tours ;
--   - pas plus de maxEnnemisActifs à l'écran ;
--   - la position horizontale et le motif de déplacement sont pseudo-aléatoires.
genererEnnemiSiBesoin :: Moteur -> Moteur
genererEnnemiSiBesoin m
  | not (doitGenererEnnemi m) = m
  | otherwise =
      let rng0 = mRng m
          (x, rng1) =
            randomR (xMinApparitionEnnemi, xMaxApparitionEnnemi) rng0

          (motifId, rng2) =
            randomR (0 :: Int, 2) rng1

      in case creerEnnemiSpawn x motifId of
           Nothing ->
             m { mRng = rng2 }

           Just ennemi ->
             m { mEnnemis = mEnnemis m ++ [ennemi]
               , mRng = rng2
               }

doitGenererEnnemi :: Moteur -> Bool
doitGenererEnnemi m =
     mTour m > 0
  && mTour m `mod` periodeApparitionEnnemi == 0
  && length (mEnnemis m) < maxEnnemisActifs

creerEnnemiSpawn :: Int -> Int -> Maybe Ennemi
creerEnnemiSpawn x motifId =
  case mkRectangle x yApparitionEnnemi largeurEnnemiSpawn hauteurEnnemiSpawn of
    Left _ ->
      Nothing

    Right h ->
      case (mkPV pvEnnemiSpawn, mkCadence cadenceTirEnnemiSpawn) of
        (Right pv, Right cadTir) ->
          case mkEnnemi h pv (motifEnnemiSpawn motifId) cadTir of
            Right ennemi -> Just ennemi
            Left _       -> Nothing

        _ ->
          Nothing

motifEnnemiSpawn :: Int -> Oracle
motifEnnemiSpawn motifId =
  case motifId `mod` 3 of
    0 ->
      Scripted
        [ Deplacer Bas
        , Attendre
        , Tirer
        , Deplacer Gauche
        , Deplacer Bas
        , Tirer
        , Deplacer Droite
        , Attendre
        ]
        0

    1 ->
      Scripted
        [ Deplacer Bas
        , Deplacer Droite
        , Tirer
        , Deplacer Bas
        , Attendre
        , Deplacer Gauche
        , Tirer
        , Attendre
        ]
        0

    _ ->
      Scripted
        [ Attendre
        , Deplacer Bas
        , Tirer
        , Deplacer Gauche
        , Deplacer Bas
        , Deplacer Droite
        , Tirer
        , Attendre
        ]
        0


prop_pre_genererEnnemiSiBesoin :: Moteur -> Bool
prop_pre_genererEnnemiSiBesoin =
  prop_inv_moteur

prop_post_genererEnnemiSiBesoin :: Moteur -> Moteur -> Bool
prop_post_genererEnnemiSiBesoin m m' =
     prop_inv_moteur m'
  && length (mEnnemis m') <= maxEnnemisActifs
  && length (mEnnemis m') >= length (mEnnemis m)