destutter (destutter xs) == destutter xs

destutter []  = []
destutter [x] = [x]
destutter (x:y:xs)       
    | x == y    =   destutter (y : xs)      
    | otherwise =   x : destutter (y : xs)

Notice monotonicity: length (destutter xs) <= length xs 

using this destutter function definition :

(1) destutter []  = []  
(2) destutter [x] = [x] 
(3) destutter (x:y:xs) =     (y:xs)  -- x == y
(4) destutter (x:y:xs) = x : (y:xs)  -- x /= y
(5) lemma destutter (x:xs) = x : f x xs

{--Proof by alternative implementation of destutter

destutter' []  = []
destutter' [x] = [x]
destutter' (x:xs) = x : f x xs
f x xs = case xs of  
                  []            -> []
                  [n]    
                   |  n == x    -> [] 
                   |  otherwise -> [n]
                  (n:z:zs) 
                   | x == n     -> f x (z:zs) 
                   | otherwise  -> n : f n (z:zs) --}





-- check  first step

(6) assumption: destutter (destutter xs) == destutter xs
(7) statement: destutter (destutter (x : xs)) == destutter (x : xs)

-- Split in two cases
-- x == head xs

{using (3) destutter (x : y : ys) = if x == y, destutter (y : ys)}

destutter (destutter (y : xs)) == destutter (y : xs) 

{using (7) and (6)}

destutter xs == destutter xs

-- x /= head xs

{using (4) destutter (x : y : ys) = if x /= y, destutter x : (y : ys)}

destutter (x : (destutter xs)) ==  x : destutter xs

(8) assumption xs = (n : ns)

{using (8)}

destutter (x : (destutter xs)) ==  x : destutter (n : ns)

{using (6) in reverse}

destutter (x : (destutter xs)) ==  x : destutter (destutter (n : ns))

{using (5) }

destutter (x : (destutter xs)) ==  x : destutter (n : f n ns)

{using (3) in reverse}

destutter (x : (destutter xs)) ==  destutter (x : n : f n ns)

{using (5) in reverse }

destutter (x : (destutter xs)) ==  destutter (x: destutter (n : ns))

{using (8) in reverse }

destutter (x : (destutter xs)) ==  destutter (x : destutter xs)


-- check case xs == []

destutter (destutter []) == destutter []
{using (1)}
destutter [] == destutter []

-- check case xs == [x]

destutter (destutter [x]) == destutter [x]
{using (2)}
destutter [x] == destutter [x]




