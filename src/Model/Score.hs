{-# LANGUAGE InstanceSigs #-}

module Model.Score where

---------------------------------------------------------------------------------
-- Score
---------------------------------------------------------------------------------

newtype Score = Score
  { scoreValeur :: Int
  } deriving (Eq, Show, Ord)

instance Semigroup Score where
  (<>) :: Score -> Score -> Score
  Score a <> Score b = Score (a + b)

instance Monoid Score where
  mempty :: Score
  mempty = Score 0

scoreNul :: Score
scoreNul =
  mempty

scoresNuls :: Int -> [Score]
scoresNuls n
  | n <= 0    = []
  | otherwise = replicate n mempty

prop_inv_score :: Score -> Bool
prop_inv_score score =
  scoreValeur score >= 0

prop_inv_scores :: [Score] -> Bool
prop_inv_scores =
  all prop_inv_score

ajouterPoints :: Int -> Score -> Score
ajouterPoints points score
  | points <= 0 = score
  | otherwise   = score <> Score points

-- | Ajoute des points au score d'une joueuse precise.
--   Si l'indice est invalide, la liste de scores est laissee inchangee.
ajouterPointsJoueuse :: Int -> Int -> [Score] -> [Score]
ajouterPointsJoueuse indice points scores
  | indice < 0 = scores
  | otherwise  = ajouterAux scores indice
  where
    ajouterAux [] _ =
      []

    ajouterAux (s:ss) 0 =
      ajouterPoints points s : ss

    ajouterAux (s:ss) i =
      s : ajouterAux ss (i - 1)

scoreTotal :: [Score] -> Score
scoreTotal =
  mconcat

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
  prop_inv_score score ==>
    prop_inv_score (ajouterPoints points score)
  where
    a ==> b = not a || b

prop_post_ajouterPointsJoueuse :: Int -> Int -> [Score] -> Bool
prop_post_ajouterPointsJoueuse indice points scores =
  prop_inv_scores scores ==>
    prop_inv_scores (ajouterPointsJoueuse indice points scores)
  where
    a ==> b = not a || b

prop_post_scoreTotal :: [Score] -> Bool
prop_post_scoreTotal scores =
  prop_inv_scores scores ==>
    prop_inv_score (scoreTotal scores)
  where
    a ==> b = not a || b

prop_scoreEnnemiDetruit_positif :: Int -> Bool
prop_scoreEnnemiDetruit_positif tour =
  scoreEnnemiDetruit tour >= scoreEnnemiBase

prop_score_semigroup_assoc :: Score -> Score -> Score -> Bool
prop_score_semigroup_assoc a b c =
  a <> (b <> c) == (a <> b) <> c

prop_score_monoid_left :: Score -> Bool
prop_score_monoid_left s =
  mempty <> s == s

prop_score_monoid_right :: Score -> Bool
prop_score_monoid_right s =
  s <> mempty == s
