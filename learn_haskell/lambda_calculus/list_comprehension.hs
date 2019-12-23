{- List comprehension: collection manipulation
* imperative langs: for, while, do-while break continue
* mathematics: 
    - set theory (computationally awkward, equality)
    - simplified to work over lists (bare minimum: head/tail)
    - {x^2 | x \in [1..5]}
* Haskell
    - x <- [1..5] called generator (defines how to gen values for x)
* SQL is basically it
-}

import Data.Char

{-
1. Implement with three methods
    * Functions to square up a list of numbers
    * Element by element
-}
-- using list comprehension
square :: Num a => [a] -> [a]
square [] = []
square xs = [x*x | x <- xs]

-- using recursion
square' :: Num a => [a] -> [a]
square' [] = []
square' (x:xs) = [x*x] ++ square'(xs)

-- using map (easy example)
square'' :: Num a => [a] -> [a]
square'' [] = []
square'' xs = map (\x -> x*x) xs

{-
2. Implement cartesian product
    * think of it as a nested loops
    * values of most deeply nested change more frequently
-}
-- using list comprehension
cartesian :: [a] -> [b] -> [(a, b)]
cartesian xs ys = [(x, y) | x <- xs, y <- ys]

-- using recursion and map
cartesian' :: [a] -> [b] -> [(a, b)]
cartesian' [] [] = []
cartesian' [] _  = []
cartesian' _ []  = []
cartesian' (x:xs) ys = map (\y -> (x, y)) ys ++ cartesian' xs ys


{- Challenge:
 Implement map and filter using recursion
-}
map' :: (a -> b) -> [a] -> [b]
map' _ []  = []
map' fn (x:xs) = [fn x] ++ map fn xs

filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' fn (x:xs) | fn x == True  = [x] ++ filter' fn xs
                  | fn x == False = [] ++ filter' fn xs
                  | otherwise     = []

-- dependent generators
-- list comprehensions can be also nested
orderedPairs :: Ord b => [b] -> [b] -> [(b, b)]
orderedPairs xs ys = 
    [(x, y) | x <- xs
            , y <- ys
            , x <= y -- filters and guards, even x
    ]

-- using recursion
orderedPairs' :: Ord b => [b] -> [b] -> [(b, b)]
orderedPairs' [] [] = []
orderedPairs' _ []  = []
orderedPairs' [] _  = []
orderedPairs' (x:xs) ys = 
    map (\y -> (x, y)) ys' ++ orderedPairs' xs ys 
        where ys' = filter (\z -> x <= z) ys

-- with list comprehension (basically what concat does)
flattenList :: [[a]] -> [a]
flattenList xss = [x | xs <- xss, x <- xs]

-- with recursion
flattenList' :: [[a]] -> [a]
flattenList' [[]] = []
flattenList' []   = []
flattenList' [xs] = xs
flattenList' (xs:xss) = flattenList' [xs] ++ flattenList' xss 


factors :: Int -> [Int]
factors n | n /= 0    = fct
          | otherwise = [1]
            where 
                fct = [x | x <- [1..n*(signum n)], n `mod` x == 0]

prime :: Int -> Bool
prime n = factors n == [1, n]

perfectNumbers :: Int -> [Int]
perfectNumbers n = [ x | x <- [1..n], isPerfect x]
    where isPerfect num = sum (init (factors num)) == num


{- Implement zip from scratch
  * Using recursion
-}
zip' :: [a] -> [b] -> [(a, b)]
zip' [] _ = []
zip' _ [] = []
zip' (x:xs) (y:ys) = [(x, y)] ++ zip' xs ys


-- using zip
adjacentPairs :: [a] -> [(a, a)]
adjacentPairs xs = zip xs (tail xs)

-- using recursion
adjacentPairs' :: [a] -> [(a, a)]
adjacentPairs' [] = []
adjacentPairs' (x:xs) = pair ++ adjacentPairs' xs
    where 
        pair = if not (null xs) then [(x, head xs)] else []

{- Having practiced the recursion
    * Move towards more high-level stuff
    * List of all position of a value in a list
-}
sorted :: Ord a => [a] -> Bool
sorted xs = 
    and [x <= y | (x, y) <- adjacentPairs xs]

positions :: Eq a => a -> [a] -> [Int]
positions val xs = [i | (x, i) <- zip xs [0..n], x == val]
    where n = length xs - 1

sumSquares :: Num a => [a] -> a
sumSquares xs = sum [x * x | x <- xs]

replicate' :: Int -> a -> [a]
replicate' n x = [x | _ <- [1..n]]

pythTriples :: (Num c, Enum c, Ord c) => c -> [(c, c, c)]
pythTriples n = 
    [(x, y, z) | x <- [1..n], y <- [1..n], z <- [1..n]
               , x * x + y * y == z * z
               , x <= y  -- eliminate 'duplicates'
    ]

scalarproduct :: [ Int ] -> [ Int ] -> Int
scalarproduct xs ys = sum [ x*y | (x, y) <- zip xs ys]


{-
Implement Caesar chypher
-}

let2int :: Char -> Int
let2int c = ord c - ord 'a'

int2let :: Int -> Char
int2let n = chr (ord 'a' + n)

shift :: Int -> Char -> Char
shift n c
    | isLower c = int2let ((let2int c + n) `mod` 26)
    | otherwise = c 

encode :: Int -> String -> String
encode n xs = [shift n x | x <- xs]