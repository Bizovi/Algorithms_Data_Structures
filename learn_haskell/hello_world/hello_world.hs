{- Trying out some of the features of the language
1. Looking up a value in a list is O(n)
2. But we need to step outside that way of thinking 
-}
double :: Num a => a -> a
double x = x + x

quadruple :: Num a => a -> a
quadruple x = double (double x)

factorial :: (Num a, Enum a) => a -> a
factorial nr = product [1..nr]

-- backticks syntactic sugar for x `R` y, which is R x y
averageFloor :: Foldable t => t Int -> Int
averageFloor ns = sum ns `div` length ns

my_value :: Int
my_value = factorial (averageFloor [32..53])

-- implicit grouping, currying
netSalaryPromo :: Fractional a => a -> a -> a
netSalaryPromo salary tax_rate = 
    salary - total_tax + promotion
        where
            total_tax = tax_rate * salary
            promotion = salary * 0.1

{- 
ex 1: Fix the syntax errors in the program
=====
* N -> lowercase
* 'div' -> `div`
* a, xs aligned
-}
n :: Int
n = a `div` length xs
        where
            a = 10
            xs = [1..5] :: [Int]

{- 
ex 2: Implement the last xs function
=====
* xs !! length xs
* How can we get rid of parantheses like in Elm?
* Flow package for pipe operators?
* Polymprphic function [a]: 
    type of elements doesn't matter
-}
last_ :: [a] -> a
last_ xs = 
    head (reverse xs)

-- recreating init function
dropLast :: [a] -> [a]
dropLast xs =
    reverse (drop 1 (reverse xs))

-- implement the length function    
length' :: [a] -> Int
length' []     = 0
length' (_:xs) = 1 + length' xs

-- implement the reverse function
reverse' :: [a] -> [a]
reverse' []     = []
reverse' (x:xs) = reverse' xs ++ [x]

-- implement the take function
take' :: Int -> [a] -> [a]
take' 0 _      = []
take' _ []     = []
take' k (x:xs) = [x] ++ take' k' xs
    where k' = k - 1

-- ugly, but w/o syntactic sugar of where
-- idea of numpy of selecting last values from array
take'' :: Int -> [a] -> [a]
take'' 0 _      = []
take'' _ []     = []
take'' k (x:xs) = [x] ++ take'' ((\k' -> k' - 1) k) xs

-- head and tail are trivial consequence of (_:_)
-- what about tail as in Python or R
-- this is ugly, would be more elegant to just drop
tailR :: Int -> [a] -> [a]
tailR k xs = reverse' (take' k (reverse' xs))

{-
ex3: Sum Integers with recursion
* Product using pattern matching
-}
sumNumbers :: Num a => [a] -> a
sumNumbers xs = doSum 0 xs
    where
        doSum total []     = total
        doSum total [y]    = total + y
        doSum total (y:ys) = doSum (total + y) ys

productNumbers :: Num a => [a] -> a
productNumbers [] = 1
productNumbers (x:xs) = x * productNumbers xs


{- 
ex4:
Implement quicksort recursively: 
Ord is for ordered
-}
qsort :: Ord a => [a] -> [a]
qsort []     = [] -- base case
qsort (x:xs) = qsort smaller ++ [x] ++ qsort larger
    where
        smaller = [ a | a <- xs, a <= x ]
        larger  = [ b | b <- xs, b > x ]


{-
Conditionals: always have then else branch
Guarded equations
-}
abs' :: (Num a, Ord a) => a -> a
abs' n | n >= 0    = n
       | otherwise = -n

signum' :: (Num a, Ord a) => a -> a
signum' n = if n < 0 then -1 else
    if n == 0 then 0 else 1


signum'' :: (Num a, Ord a) => a -> a
signum'' n | n < 0     = 0
           | n == 0    = 0
           | otherwise = 1
