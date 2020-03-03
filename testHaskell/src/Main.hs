module Main where
import Test.QuickCheck
import Data.List

-- 
main :: IO ()
main = do
  putStrLn "I have to use this, because my ghci can't see QuickCheck"

-- CASE 1
-- Let's think about double reversing 
-- I'm pretty sure that tests should be succssful
doubleReverse :: [Integer] -> Bool
doubleReverse xs = reverse (reverse xs) == xs
-- *Main> quickCheck doubleReverse 
-- +++ OK, passed 100 tests.
-- It seems that everything is fine here


-- CASE 2
-- Let's think about reverse concatanating vs concatanating reverses 
-- I have some doubts
concatReverse :: [Integer] -> [Integer] -> Bool
concatReverse xs ys = reverse (xs ++ ys) == reverse ys ++ reverse xs
-- *Main> quickCheck concatReverse 
-- +++ OK, passed 100 tests.
-- Nope, everything was fine here too

-- CASE 3
-- Proving that (++) preserves the length 
-- No doubts, we just prooved it by induction
lengthPreserve :: [Integer] -> [Integer] -> Bool
lengthPreserve xs ys = length (xs ++ ys) == length xs + length ys
-- *Main> quickCheck lengthPreserve 
-- +++ OK, passed 100 tests.
-- Perfect, but I remember, that tests are not proper prooving

-- CASE 4
-- Let's take offered task
-- I'm sure it's not correct, because unlines always add a new line to the string
prop_unlines_lines = \x -> unlines (lines x) == x
-- *** Failed! Falsified (after 3 tests and 1 shrink):     
-- "a"
-- And it obviously failed


-- CASE 5
-- Let's test domething more tricky for QuickCheck
-- Winks got here http://matt.might.net/articles/quick-quickcheck/
-- We can encode the notProduct relation 
-- (which says than some number is not the product of two others) 
-- to hijack QuickCheck for (rather inefficient) prime factorization:

notProduct :: Int -> Int -> Int -> Bool
notProduct n p q = n /= p * q

-- first try we had
-- *Main> quickCheck(notProduct 10)
-- +++ OK, passed 100 tests.
-- but second try
-- *** Failed! Falsified (after 15 tests):                  
-- -10
-- -1
-- Bingo, finally the test failed


-- CASE 6
-- Sort is idempotent function, so double sorting 
-- should be equal to single sort
sortDoublesort :: Ord a => [a] -> Bool
sortDoublesort xs = sort (sort xs) == sort xs
-- *Main> quickCheck sortDoublesort 
-- +++ OK, passed 100 tests.
-- Yeah, thats pretty easy 

-- CASE 7
-- Let's compare different sorting
-- Our own quicksort and Data.List proper (looks like merge sort)
quicksort :: (Ord a) => [a] -> [a]  
quicksort [] = []  
quicksort (x:xs) =   
    let smallerSorted = quicksort [a | a <- xs, a <= x]  
        biggerSorted = quicksort [a | a <- xs, a > x]  
    in  smallerSorted ++ [x] ++ biggerSorted  

sortMethods xs =  quicksort xs == sort xs 
-- *Main> quickCheck sortMethods
-- +++ OK, passed 100 tests.
-- Sort always sort, regardless of the method

-- CASE 8
-- Proved by induction, pretty complicated
mapConcat (Blind f) x y = map f x ++ map f y == map f (x++y)
-- *Main> quickCheck mapConcat 
-- +++ OK, passed 100 tests.
-- It's confirmed
-- The most difficult here was to find, that I should use
-- Blind to hide function

-- CASE 9
-- I suppose that group is a monotonic function
-- so lenght before group should not be less, then after
monotonicGroup xs = length xs >= length ( group xs )
-- *Main> quickCheck monotonicGroup 
-- +++ OK, passed 100 tests.
-- It has been passed

-- CASE 10
-- I used 'zip' only once, should it be commutative
-- with 'unzip'?
zipNunzip :: [Char] -> [Int] -> Bool
zipNunzip xs ys = fst (unzip (zip xs ys)) == xs
-- *** Failed! Falsified (after 2 tests and 1 shrink):     
-- "a"
-- []
-- failed, it seems like zipping non empty list
-- with empty list returns empty in unzipping 


-- CASE 11
-- Testing 'transpose' as we remember (A^-T)^-T == A
testTranspose xs = transpose (transpose xs) == xs
-- *Main> quickCheck testTranspose 
-- *** Failed! Falsified (after 6 tests and 2 shrinks):    
-- [[]]
-- Oops, what a bad surprise
-- I forgot zero vector

-- CASE 12
-- Now I'll hang it!
-- the number of permutations should should be more or the
-- same of the number of base elements 
-- I'm thinking, after few steps it will hang. O(n!)
testPermutations xs = length (permutations xs) >= length xs
-- Main> quickCheck testPermutations 
-- (13 tests)
-- Yeps, after 13 steps ))


destutter' [] = []                                          
destutter' (x:xs) = x : (destutter' (dropWhile (== x) xs))


destutter :: Eq a => [a] -> [a]     
destutter []  = []   -- 1     
destutter [x] = [x]  -- 2     
destutter (x:xs) = x : f x xs
f x xs = case xs of  
                  []            -> []
                  [n]    
                   |  n == x    -> [] 
                   |  otherwise -> [n]
                  (n:z:zs) 
                   | x == n     -> f x (z:zs) 
                   | otherwise  -> n : f n (z:zs) 

testDes xs = destutter xs == destutter' xs

