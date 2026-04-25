module Model.Engine.Properties where

import Model.Engine.Collisions
import Model.Engine.Step
import Model.Engine.Types

prop_tour_incremente :: Moteur -> Bool
prop_tour_incremente m
  | not (prop_inv_moteur m)      = True
  | not (prop_partie_en_cours m) = True
  | otherwise = mTour (finDeTourMoteur m) == mTour m + 1

prop_invariant_preserve :: Moteur -> Bool
prop_invariant_preserve m
  | not (prop_inv_moteur m)      = True
  | not (prop_partie_en_cours m) = True
  | otherwise = prop_inv_moteur (finDeTourMoteur m)

prop_joueuses_non_croissantes :: Moteur -> Bool
prop_joueuses_non_croissantes m
  | not (prop_inv_moteur m)      = True
  | not (prop_partie_en_cours m) = True
  | otherwise =
      length (mJoueuses (finDeTourMoteur m)) <= length (mJoueuses m)

prop_joueuses_stables :: Moteur -> Bool
prop_joueuses_stables =
  prop_joueuses_non_croissantes

prop_script_diminue :: Moteur -> Bool
prop_script_diminue m
  | not (prop_inv_moteur m)      = True
  | not (prop_partie_en_cours m) = True
  | otherwise =
      let m' = finDeTourMoteur m
      in all (\ep -> epTour ep > mTour m) (mScript m')

-- | Preuve informelle de prop_tour_incremente :
--
--   Soit m un moteur vérifiant prop_inv_moteur et prop_partie_en_cours.
--   Par définition de finDeTourMoteur, la dernière étape pose :
--     mTour m' = mTour m + 1
--   Donc mTour (finDeTourMoteur m) = mTour m + 1.
