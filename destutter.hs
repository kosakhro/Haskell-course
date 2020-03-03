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
-- case 4 we may assume and claim
Assumption destutter   (x:xs) =  x:destutter   (xs)
Claim      destutter (y:x:xs) =  y:destutter (x:xs)

-- check case xs == []

destutter (destutter []) == destutter []
{using (1)}
destutter [] == destutter []

-- check case xs == [x]

destutter (destutter [x]) == destutter [x]
{using (2)}
destutter [x] == destutter [x]

-- check other cases

destutter (destutter (x:y:xs)) == destutter (x:y:xs)

-- Split in two cases
-- x == y

{using (3)}

destutter (destutter (y:xs)) == destutter (y:xs) -- and we step up

-- x /= y

{using (4) and Claim}

destutter x : (destutter (y:xs)) == x : destutter (y:xs)

{using (4) and Assumption}

x : destutter (destutter (y:xs)) == x : destutter (y:xs) -- and we step up

-- for a finite set, taking into account that function is monotone decreasing
--  we have proved 



