module Model.Engine.ListUtils where

-- | Suppression par indice. Si l'indice est invalide, la liste est inchangée.
removeAt :: Int -> [a] -> [a]
removeAt i xs
  | i < 0     = xs
removeAt _ [] = []
removeAt 0 (_:rest) = rest
removeAt n (x:rest) = x : removeAt (n - 1) rest

-- | Recherche sûre d'un élément par indice.
lookupAt :: Int -> [a] -> Maybe a
lookupAt i _ | i < 0 = Nothing
lookupAt _ []        = Nothing
lookupAt 0 (x:_)     = Just x
lookupAt n (_:xs)    = lookupAt (n - 1) xs

-- | Remplacement sûr d'un élément par indice.
replaceAt :: Int -> a -> [a] -> [a]
replaceAt i _ xs
  | i < 0     = xs
replaceAt _ _ [] = []
replaceAt 0 x (_:xs) = x : xs
replaceAt n x (y:ys) = y : replaceAt (n - 1) x ys
