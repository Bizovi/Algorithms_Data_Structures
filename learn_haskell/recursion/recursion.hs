{- A deep dive on recursion
* x:xs (tail call elimination), don't keep the full stack
* very important in control structures
* this is why we can do it in Haskell
* proofs by induction
-}

-- very similar to list recursions
factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n-1)

drop' :: Int -> [a] -> [a]
drop' 0 xs = xs
drop' _ [] = []
drop' n (_:xs) = drop' (n - 1) xs

appendLists :: [a] -> [a] -> [a]
appendLists [] ys = ys
appendLists (x:xs) ys = x : (appendLists xs ys)

{-
* Select nth element of a list
* Decide if the value is an element of a list
-}

-- recurse over both list and integer
nElement :: [a] -> Int -> a
nElement [] _ = error "List out of bounds"
nElement (x:_)  0 = x
nElement (_:xs) n = nElement xs (n-1)

-- using recursion, trivial with map / filter / list comprehension
isElement :: Eq a => a -> [a] -> Bool
isElement _ [] = False
isElement n (x:xs) = n == x || isElement n xs

-- exponentiate
exp' :: Int -> Int -> Int
exp' _ 0 = 1
exp' 0 _ = 0
exp' k n = k * exp' k (n - 1)

-- implement vector and (for a list)
-- similarly for or (basically the math defn)
and' :: [Bool] -> Bool
and' [] = True
and' [x] = x
and' (x:xs) = x && and' xs

-- merge two sorted lists in ascending order
mergeAscending :: Ord a => [a] -> [a] -> [a]
mergeAscending [] []         = []  -- unnecessary
mergeAscending [] ys         = ys
mergeAscending xs []         = xs
mergeAscending (x:xs) (y:ys) = [first] ++ mergeAscending xs' ys'
    where
        first      = if y > x then x else y
        (xs', ys') = if y > x then (xs, (y:ys)) else ((x:xs), ys)

-- implement mergesort (sort two halves of the list separately)
halve :: [a] -> ([a], [a])
halve xs = splitAt (length xs `div` 2) xs

msort :: Ord a => [a] -> [a]
msort []  = []
msort [x] = [x]
msort xs  = mergeAscending (msort xs') (msort xs'')
    where 
        (xs', xs'') = halve xs

-- implement splitAt from scratch
splitAt' :: Int -> [a] -> ([a], [a])
splitAt' _ []     = ([], [])
splitAt' 0 xs     = ([], xs) 
splitAt' n (x:xs) = ([x] ++ xs', xs'')
    where
        (xs', xs'') = splitAt' (n-1) xs