
-- Try to fold everything

length'' :: [a] -> Int
length'' = foldr (\_ n -> n + 1) 0

concat'' :: [[a]] -> [a]
concat'' = foldr (++) []


remove'' :: Eq a => a -> [a] -> [a]
remove'' rm = g . foldr f e
    where 
      g (x,y,isRemoved) = x
      f x (xs, ys, isRemoved)
        | (x /= rm)                       = ((x:xs),(x:ys),isRemoved)
        | (x == rm) && not (isRemoved)    = ((  xs),(x:ys),True)
        | otherwise                       = ((  ys),(x:ys),isRemoved)
      e = ([],[],False)


find'' :: (a->Bool) -> [a] -> Maybe a
find'' p = foldr (\x xs -> if p x then Just x else xs) Nothing

filter'' :: (a->Bool) -> [a] -> [a]
filter'' p = foldr (\x xs -> if p x then x : xs else xs) []

take'' :: Int -> [a] -> [a]
take'' n xs = foldr count (const []) xs n
  where
    count x g 0 = []
    count x g n = x:g (n-1)

nub'' :: Eq a => [a] -> [a]
nub'' = foldr (\ x xs -> x : filter (/= x) xs) []