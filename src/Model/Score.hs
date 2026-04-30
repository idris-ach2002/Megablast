module Model.Score where

---------------------------------------------------------------------------------
-- Score
---------------------------------------------------------------------------------

newtype Score = Score
  { scoreValeur :: Int
  } deriving (Eq, Show, Ord)

scoreNul :: Score
scoreNul =
  Score 0

prop_inv_score :: Score -> Bool
prop_inv_score score =
  scoreValeur score >= 0

ajouterPoints :: Int -> Score -> Score
ajouterPoints points score
  | points <= 0 = score
  | otherwise   = Score (scoreValeur score + points)

scoreEnnemiBase :: Int
scoreEnnemiBase =
  100

-- | Multiplicateur simple basé sur le nombre de tours survécus.
--   Plus la partie dure, plus tuer un ennemi rapporte.
multiplicateurSurvie :: Int -> Int
multiplicateurSurvie tour
  | tour < 1000 = 1
  | tour < 2500 = 2
  | tour < 5000 = 3
  | otherwise   = 4

scoreEnnemiDetruit :: Int -> Int
scoreEnnemiDetruit tour =
  scoreEnnemiBase * multiplicateurSurvie tour

prop_post_ajouterPoints :: Int -> Score -> Bool
prop_post_ajouterPoints points score =
  prop_inv_score (ajouterPoints points score)

prop_scoreEnnemiDetruit_positif :: Int -> Bool
prop_scoreEnnemiDetruit_positif tour =
  scoreEnnemiDetruit tour >= scoreEnnemiBase