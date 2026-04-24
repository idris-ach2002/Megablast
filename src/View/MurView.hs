module View.MurView where

import Graphics.Gloss
import Model.Engine (MursNiveau(..))
import Model.Hitbox
import qualified View.HitboxView as HB
import qualified View.Theme as Theme

---------------------------------------------------------------------------------
-- Rendu spécialisé des murs
---------------------------------------------------------------------------------

margeVisibleMur :: Int
margeVisibleMur = 20

visibleYMin, visibleYMax :: Int
visibleYMin = -margeVisibleMur
visibleYMax = HB.hauteurFenetre + margeVisibleMur

-- | Dessine les deux murs du niveau en ne gardant que la portion visible.
dessinerMursNiveau :: MursNiveau -> Picture
dessinerMursNiveau murs =
  pictures
    [ dessinerMurVisible Theme.couleurMurGauche (mnMurGauche murs)
    , dessinerMurVisible Theme.couleurMurDroit  (mnMurDroit murs)
    ]

-- | Rendu spécialisé pour les murs. On ne parcourt que les segments utiles à la
-- fenêtre visible afin d'éviter d'essayer d'afficher une liste infinie.
dessinerMurVisible :: Color -> Hitbox -> Picture
dessinerMurVisible c (MurGauche pts) =
  pictures $ concatMap (dessinerSegmentGaucheVisible c) (segmentsVisibles pts)
dessinerMurVisible c (MurDroit pts) =
  pictures $ concatMap (dessinerSegmentDroitVisible c) (segmentsVisibles pts)
dessinerMurVisible c h = HB.dessinerHitbox c h

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
      in Just ((xOnSegmentAtYInt yStart seg, yStart), (xOnSegmentAtYInt yEnd seg, yEnd))

xOnSegmentAtYInt :: Int -> ((Int, Int), (Int, Int)) -> Int
xOnSegmentAtYInt y seg = round (xOnSegmentAtYFloat y seg)

xOnSegmentAtYFloat :: Int -> ((Int, Int), (Int, Int)) -> Float
xOnSegmentAtYFloat y ((x1, y1), (x2, y2))
  | y1 == y2  = fromIntegral x1
  | otherwise =
      let t = fromIntegral (y - y1) / fromIntegral (y2 - y1) :: Float
      in fromIntegral x1 + t * fromIntegral (x2 - x1)

murAlpha :: Color -> Color
murAlpha c = withAlpha 0.70 c

murBord :: Color -> Color
murBord c = mixColors 0.35 0.65 white c

bordGaucheFenetre, bordDroitFenetre :: Float
bordGaucheFenetre = fromIntegral (- HB.largeurFenetre `div` 2)
bordDroitFenetre  = fromIntegral (HB.largeurFenetre `div` 2)

dessinerSegmentGaucheVisible :: Color -> ((Int, Int), (Int, Int)) -> [Picture]
dessinerSegmentGaucheVisible c ((x1, y1), (x2, y2)) =
  let (gx1, gy1) = HB.toGloss x1 y1
      (gx2, gy2) = HB.toGloss x2 y2
      fond = color (murAlpha c) $
        polygon [(bordGaucheFenetre, gy1), (gx1, gy1), (gx2, gy2), (bordGaucheFenetre, gy2)]
      bord = color (murBord c) $ line [(gx1, gy1), (gx2, gy2)]
  in [fond, bord]

dessinerSegmentDroitVisible :: Color -> ((Int, Int), (Int, Int)) -> [Picture]
dessinerSegmentDroitVisible c ((x1, y1), (x2, y2)) =
  let (gx1, gy1) = HB.toGloss x1 y1
      (gx2, gy2) = HB.toGloss x2 y2
      fond = color (murAlpha c) $
        polygon [(gx1, gy1), (bordDroitFenetre, gy1), (bordDroitFenetre, gy2), (gx2, gy2)]
      bord = color (murBord c) $ line [(gx1, gy1), (gx2, gy2)]
  in [fond, bord]
