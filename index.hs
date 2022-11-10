import Data.Bitraversable (Bitraversable)
{-
    EECS 368 Assignment 6
    Defined the following 5 Haskell Functions:
        - replicate
        - perfects
        - find
        - positions
        - scalarproduct
-}

{-
    REPLICATE
    INPUTS: a int representing the length of an array and a value
    OUTPUTS: a list of size length where every element is value
-}
replicate :: Int -> a -> [a]
replicate n v = [v | x <- [1..n]]

{-
    PERFECTS
    INPUTS: an integer n
    OUTPUTS: a list of every perfect integer from 1 to n
-}
-- helper function - finds all factors of n up to n-1
factors :: Int -> [Int]
factors n = [x | x <- [1..n-1], n `mod` x == 0]
perfects :: Int -> [Int]
perfects n = [x | x <- [1..n], sum (factors x) == x]


{-
    FIND
    INPUTS: an identifier and a list of key-value pairs
    OUTPUTS: a list where each element is value if identifier == key
-}
find :: Eq a => a -> [(a, b)] -> [b]
find i xs = [b | (a, b) <- xs, a == i]

{-
    POSITIONS
    INPUTS: an identifier and a list of values
    OUTPUTS: a list of integers that holds the indexes where identifier == value
-}
positions :: Eq a => a -> [a] -> [Int]
positions v xs = find v (zip xs [0..length xs]) 

{-
    SCALARPRODUCT
    INPUTS: two lists of integers
    OUTPUTS: an int which is the scalar product of the two lists
-}
scalarproduct :: [Int] -> [Int] -> Int
scalarproduct xs ys = sum [a * b | (a, b) <- zip xs ys]
