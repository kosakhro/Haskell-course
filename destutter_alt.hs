destutter (destutter xs) == destutter xs

-- alternative sketch of the proof
-- using modified function


destutter []  = []
destutter [x] = [x]
destutter (x:xs) = x : f x xs
f x xs = case xs of  
                  []            -> []
                  [n]    
                   |  n == x    -> [] 
                   |  otherwise -> [n]
                  (n:z:zs) 
                   | x == n     -> f x (z:zs) 
                   | otherwise  -> n : f n (z:zs) 

-- cases [], [x] are trivial

-- case (x:xs)

destutter (destutter (x:xs)) == destutter (x:xs)

destutter (x : f x xs) = x : f x xs

-- we need Assumption f x (f x xs) = f x xs


x : f x (f x xs) = x : f x xs

case xs = [] || [x]
x : f x [] = x : []
x : [] = x : []

case xs = [n]
x : f x [n] = x : [n]
x : [n] = x : [n]


case xs of (n:z:zs), n == x
x : f x (f x (z:zs)) == x : f x (z:zs)
{-by Assumption -} 
x : f x (z:zs) == x : f x (z:zs)


case xs of (n:z:zs), n /= x
x : f x (n : f n (z : zs)) = x : n : f n (z : zs) 

-- let's use this f n (z : zs) = ts

x : f x (n : ts) = x : n : ts

{- because x /= n -} 

x : n : f n (ts) = x : n : ts

x : n : f n (f n (z : zs)) = x : n : f n (z : zs)

{-by Assumption -} 

x : n : f n (z : zs) = x : n : f n (z : zs)

-- cases [], [x], [n]

