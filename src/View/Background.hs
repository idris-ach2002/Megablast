module View.Background
  ( dessinerFondEspace
  ) where

import Graphics.Gloss
import System.Random (mkStdGen, randomRs)

import qualified View.HitboxView as HB
import qualified View.Theme as Theme
import Data.List (zipWith4)

---------------------------------------------------------------------------------
-- Fond spatial
---------------------------------------------------------------------------------

data Etoile = Etoile
  { eX       :: Int
  , eY       :: Int
  , eTaille  :: Float
  , eVitesse :: Int
  } deriving (Eq, Show)

nombreEtoiles :: Int
nombreEtoiles = 120

graineFondEspace :: Int
graineFondEspace = 2048

etoiles :: [Etoile]
etoiles =
  genererEtoiles graineFondEspace nombreEtoiles

genererEtoiles :: Int -> Int -> [Etoile]
genererEtoiles graine n =
  take n $
    zipWith4 Etoile xs ys tailles vitesses
  where
    xs =
      randomRs (0, HB.largeurFenetre - 1) (mkStdGen graine)

    ys =
      randomRs (0, HB.hauteurFenetre - 1) (mkStdGen (graine + 1))

    tailles =
      map tailleDepuisEntier $
        randomRs (0, 100 :: Int) (mkStdGen (graine + 2))

    vitesses =
      randomRs (6, 22) (mkStdGen (graine + 3))

tailleDepuisEntier :: Int -> Float
tailleDepuisEntier n
  | n < 70    = 0.8
  | n < 92    = 1.2
  | otherwise = 1.8

dessinerFondEspace :: Int -> Picture
dessinerFondEspace tour =
  pictures $
    dessinerCiel
    : map (dessinerEtoile tour) etoiles

dessinerCiel :: Picture
dessinerCiel =
  color Theme.couleurFond $
    rectangleSolid
      (fromIntegral HB.largeurFenetre)
      (fromIntegral HB.hauteurFenetre)

dessinerEtoile :: Int -> Etoile -> Picture
dessinerEtoile tour etoile =
  let yAnime =
        positionYEtoile tour etoile

      (gx, gy) =
        HB.toGloss (eX etoile) yAnime

      couleur =
        couleurEtoile etoile

  in color couleur $
       translate gx gy $
         formeEtoile (eTaille etoile)

positionYEtoile :: Int -> Etoile -> Int
positionYEtoile tour etoile =
  (eY etoile - tour `div` eVitesse etoile) `mod` HB.hauteurFenetre

couleurEtoile :: Etoile -> Color
couleurEtoile etoile
  | eTaille etoile < 1.0 = Theme.couleurEtoileFaible
  | otherwise            = Theme.couleurEtoile

formeEtoile :: Float -> Picture
formeEtoile taille
  | taille < 1.5 =
      circleSolid taille
  | otherwise =
      pictures
        [ circleSolid taille
        , line [(-taille * 2.5, 0), (taille * 2.5, 0)]
        , line [(0, -taille * 2.5), (0, taille * 2.5)]
        ]