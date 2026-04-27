module View.MurView where

import Graphics.Gloss
import Model.Engine (MursNiveau(..))
import Model.Hitbox
import qualified View.HitboxView as HB
import View.Assets
import View.Theme

---------------------------------------------------------------------------------
-- Rendu spécialisé des murs
---------------------------------------------------------------------------------

margeVisibleMur :: Int
margeVisibleMur = 20

visibleYMin, visibleYMax :: Int
visibleYMin = -margeVisibleMur
visibleYMax = HB.hauteurFenetre + margeVisibleMur

---------------------------------------------------------------------------------
-- Façade avec assets
--
-- On garde le paramètre AssetsView pour ne pas casser View.View/Main.
-- La forme réelle du mur reste celle de la hitbox en dent de scie.
-- Le rendu est procédural, inspiré de l'asset, mais découpé selon les segments.
---------------------------------------------------------------------------------

dessinerMursNiveauAvecAssets :: AssetsView -> Int -> MursNiveau -> Picture
dessinerMursNiveauAvecAssets _ tour =
  dessinerMursNiveauTexture tour

-- | Rendu sans assets chargés. Utilisé comme fallback.
dessinerMursNiveau :: MursNiveau -> Picture
dessinerMursNiveau =
  dessinerMursNiveauTexture 0

dessinerMursNiveauTexture :: Int -> MursNiveau -> Picture
dessinerMursNiveauTexture tour murs =
  pictures
    [ dessinerMurGaucheTexture tour (mnMurGauche murs)
    , dessinerMurDroitTexture  tour (mnMurDroit murs)
    ]

---------------------------------------------------------------------------------
-- Rendu texturé des murs
---------------------------------------------------------------------------------

dessinerMurGaucheTexture :: Int -> Hitbox -> Picture
dessinerMurGaucheTexture tour (MurGauche pts) =
  pictures $
    concatMap (dessinerSegmentGaucheTexture tour) (segmentsVisibles pts)
dessinerMurGaucheTexture _ h =
  HB.dessinerHitbox couleurMurFondSimple h

dessinerMurDroitTexture :: Int -> Hitbox -> Picture
dessinerMurDroitTexture tour (MurDroit pts) =
  pictures $
    concatMap (dessinerSegmentDroitTexture tour) (segmentsVisibles pts)
dessinerMurDroitTexture _ h =
  HB.dessinerHitbox couleurMurFondSimple h

dessinerSegmentGaucheTexture :: Int -> ((Int, Int), (Int, Int)) -> [Picture]
dessinerSegmentGaucheTexture tour seg@((x1, y1), (x2, y2)) =
  let (gx1, gy1) = HB.toGloss x1 y1
      (gx2, gy2) = HB.toGloss x2 y2

      fond =
        color (couleurMurFond (y1 + tour)) $
          polygon
            [ (bordGaucheFenetre, gy1)
            , (gx1, gy1)
            , (gx2, gy2)
            , (bordGaucheFenetre, gy2)
            ]

      ombreInterieure =
        color couleurMurOmbre $
          line [(gx1 - 3, gy1), (gx2 - 3, gy2)]

      bordInterieur =
        color couleurMurBord $
          line [(gx1, gy1), (gx2, gy2)]

  in [fond, ombreInterieure, bordInterieur]
     ++ decorSegmentGauche seg

dessinerSegmentDroitTexture :: Int -> ((Int, Int), (Int, Int)) -> [Picture]
dessinerSegmentDroitTexture tour seg@((x1, y1), (x2, y2)) =
  let (gx1, gy1) = HB.toGloss x1 y1
      (gx2, gy2) = HB.toGloss x2 y2

      fond =
        color (couleurMurFond (y1 + tour)) $
          polygon
            [ (gx1, gy1)
            , (bordDroitFenetre, gy1)
            , (bordDroitFenetre, gy2)
            , (gx2, gy2)
            ]

      ombreInterieure =
        color couleurMurOmbre $
          line [(gx1 + 3, gy1), (gx2 + 3, gy2)]

      bordInterieur =
        color couleurMurBord $
          line [(gx1, gy1), (gx2, gy2)]

  in [fond, ombreInterieure, bordInterieur]
     ++ decorSegmentDroit seg

---------------------------------------------------------------------------------
-- Décor interne des murs
---------------------------------------------------------------------------------

-- | Pour le mur gauche, on choisit une ordonnée du segment où le mur est large.
--   Ça évite de placer le décor sur les pointes de la dent de scie.
decorSegmentGauche :: ((Int, Int), (Int, Int)) -> [Picture]
decorSegmentGauche seg =
  let yDecor = yLePlusLargeGauche seg
      xBord  = xOnSegmentAtYInt yDecor seg
  in dessinerBarreGauche xBord yDecor
     ++ dessinerPastilleGauche xBord yDecor

-- | Pour le mur droit, le mur est large quand le bord intérieur est plus à gauche.
decorSegmentDroit :: ((Int, Int), (Int, Int)) -> [Picture]
decorSegmentDroit seg =
  let yDecor = yLePlusLargeDroit seg
      xBord  = xOnSegmentAtYInt yDecor seg
  in dessinerBarreDroit xBord yDecor
     ++ dessinerPastilleDroit xBord yDecor

yLePlusLargeGauche :: ((Int, Int), (Int, Int)) -> Int
yLePlusLargeGauche seg =
  meilleurY max (candidatsYSegment seg) seg

yLePlusLargeDroit :: ((Int, Int), (Int, Int)) -> Int
yLePlusLargeDroit seg =
  meilleurY min (candidatsYSegment seg) seg

meilleurY
  :: (Int -> Int -> Int)
  -> [Int]
  -> ((Int, Int), (Int, Int))
  -> Int
meilleurY _ [] ((_, y1), (_, y2)) =
  (y1 + y2) `div` 2
meilleurY choisir (y:ys) seg =
  foldl choisirMeilleur y ys
  where
    choisirMeilleur ya yb =
      let xa = xOnSegmentAtYInt ya seg
          xb = xOnSegmentAtYInt yb seg
      in if choisir xa xb == xa then ya else yb

candidatsYSegment :: ((Int, Int), (Int, Int)) -> [Int]
candidatsYSegment ((_, y1), (_, y2)) =
  [ (3 * y1 + y2) `div` 4
  , (y1 + y2) `div` 2
  , (y1 + 3 * y2) `div` 4
  ]

dessinerBarreGauche :: Int -> Int -> [Picture]
dessinerBarreGauche xBord y
  | xBord < 18 = []
  | otherwise =
      [ HB.dessinerRectangleSolid couleurBarreFond  xBar       (y - 8) largeur       16
      , HB.dessinerRectangleSolid couleurMurBord    xBar       (y - 8) largeur        2
      , HB.dessinerRectangleSolid couleurMurBord    xBar       (y + 6) largeur        2
      , HB.dessinerRectangleSolid couleurBarreMetal (xBar + 6) (y - 4) largeurInterne 8
      ]
  where
    xBar =
      4

    largeur =
      max 18 (min 48 (xBord - 8))

    largeurInterne =
      max 6 (largeur - 12)

dessinerBarreDroit :: Int -> Int -> [Picture]
dessinerBarreDroit xBord y
  | HB.largeurFenetre - xBord < 18 = []
  | otherwise =
      [ HB.dessinerRectangleSolid couleurBarreFond  xBar       (y - 8) largeur       16
      , HB.dessinerRectangleSolid couleurMurBord    xBar       (y - 8) largeur        2
      , HB.dessinerRectangleSolid couleurMurBord    xBar       (y + 6) largeur        2
      , HB.dessinerRectangleSolid couleurBarreMetal (xBar + 6) (y - 4) largeurInterne 8
      ]
  where
    largeur =
      max 18 (min 48 (HB.largeurFenetre - xBord - 8))

    xBar =
      HB.largeurFenetre - largeur - 4

    largeurInterne =
      max 6 (largeur - 12)

dessinerPastilleGauche :: Int -> Int -> [Picture]
dessinerPastilleGauche xBord y
  | xBord < 24 = []
  | otherwise =
      [ cercleJeu couleurPastilleBord    x y 11
      , cercleJeu couleurPastilleCentre  x y  6
      , cercleJeu couleurPastilleLumiere x y  2
      ]
  where
    x =
      max 14 (min 36 (xBord - 10))

dessinerPastilleDroit :: Int -> Int -> [Picture]
dessinerPastilleDroit xBord y
  | HB.largeurFenetre - xBord < 24 = []
  | otherwise =
      [ cercleJeu couleurPastilleBord    x y 11
      , cercleJeu couleurPastilleCentre  x y  6
      , cercleJeu couleurPastilleLumiere x y  2
      ]
  where
    x =
      min (HB.largeurFenetre - 14)
          (max (HB.largeurFenetre - 36) (xBord + 10))

cercleJeu :: Color -> Int -> Int -> Int -> Picture
cercleJeu c x y r =
  let (gx, gy) = HB.toGloss x y
  in color c $
       translate gx gy $
         circleSolid (fromIntegral r)

---------------------------------------------------------------------------------
-- Segments visibles
---------------------------------------------------------------------------------

segmentsVisibles :: [(Int, Int)] -> [((Int, Int), (Int, Int))]
segmentsVisibles = go
  where
    go (p1@(_, y1) : p2@(_, y2) : rest)
      | y2 < visibleYMin = go (p2 : rest)
      | y1 > visibleYMax = []
      | otherwise =
          case clipSegmentY visibleYMin visibleYMax (p1, p2) of
            Just seg -> seg : go (p2 : rest)
            Nothing  -> go (p2 : rest)
    go _ = []

clipSegmentY :: Int -> Int -> ((Int, Int), (Int, Int)) -> Maybe ((Int, Int), (Int, Int))
clipSegmentY yMin yMax seg@((_, y1), (_, y2))
  | max yMin y1 > min yMax y2 = Nothing
  | otherwise =
      let yStart = max yMin y1
          yEnd   = min yMax y2
      in Just
          ( (xOnSegmentAtYInt yStart seg, yStart)
          , (xOnSegmentAtYInt yEnd   seg, yEnd)
          )

xOnSegmentAtYInt :: Int -> ((Int, Int), (Int, Int)) -> Int
xOnSegmentAtYInt y seg =
  round (xOnSegmentAtYFloat y seg)

xOnSegmentAtYFloat :: Int -> ((Int, Int), (Int, Int)) -> Float
xOnSegmentAtYFloat y ((x1, y1), (x2, y2))
  | y1 == y2  = fromIntegral x1
  | otherwise =
      let t = fromIntegral (y - y1) / fromIntegral (y2 - y1) :: Float
      in fromIntegral x1 + t * fromIntegral (x2 - x1)

bordGaucheFenetre, bordDroitFenetre :: Float
bordGaucheFenetre =
  fromIntegral (- HB.largeurFenetre `div` 2)

bordDroitFenetre =
  fromIntegral (HB.largeurFenetre `div` 2)