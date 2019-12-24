{- foldr
* abstract map and filter using recursive structures
* r - from the right (empty list)
* l - from the left
-}

foldr' :: (a -> b -> b) -> b -> [a] -> b
foldr' _ v [] = v
foldr' f v (x:xs) = f x (foldr' f v xs)

sumFold :: Num a => [a] -> a
sumFold xs = foldr' (+) 0 xs