{-#LANGUAGE ExplicitForAll#-}

module Exercise where
import Data.List 
import Data.Ord
import Data.Char



doubleMe = \x -> x + x
doubleUs x y = doubleMe x + doubleMe y
doubleSmall x = if x > 100 
                  then x 
                  else doubleMe x

boomBang xs = [if x < 10 then "BOOM!" else "BANG" | x <- xs, odd x]

removeNonUppercase :: [Char] -> [Char]
removeNonUppercase st = [ c | c <- st, c `elem` ['A'..'Z']]

addThree :: Int -> Int -> Int -> Int
addThree a b c = a + b + c

factorial :: Integer -> Integer
factorial n = product [1..n]

lucky :: (Integral a) => a -> String
lucky 7 = "Lucky MF"
lucky x = "unhappy"

factorial2 :: (Integral a) => a -> a
factorial2 0 = 1
factorial2 n = n * factorial2 (n-1)

charName :: Char -> String
charName 'a' = "Alpha"
charName 'b' = "Beta"
charName 'c' = "Cetta"
charName x = "Kukka"


addVectors :: (Num a) => (a, a) -> (a, a) -> (a, a)
addVectors (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

first :: (a, b, c) -> a
first (x, _, _) = x

second :: (a, b, c) -> b
second (_, y, _) = y

third :: (a, b, c) -> c
third (_, _, z) = z

head' :: [a] -> a  
head' [] = error "Can't call head on an empty list, dummy!"  
head' (x:xs) = x 

tell :: (Show a) => [a] -> String  
tell [] = "The list is empty"  
tell (x:[]) = "The list has one element: " ++ show x  
tell (x:y:[]) = "The list has two elements: " ++ show x ++ " and " ++ show y  
tell (x:y:_) = "This list is long. The first two elements are: " ++ show x ++ " and " ++ show y  

length' :: (Num b) => [a] -> b
length' [] = 0
length' (_:xs) = 1 + length' xs

sum' :: (Num a) => [a] -> a
sum' [] = 0
sum' (x:xs) = x + sum' xs

bmiTell :: (RealFloat a) => a -> a -> String
bmiTell weight height
    | bmi <= 18.5 = "Eat more"
    | bmi <= 25 = "Its okay"
    | bmi <= 30 = "Stop eating"
    | otherwise = "Mammoth"
    where bmi = weight / height ^ 2


cylinder :: (RealFloat a) => a -> a -> a  
cylinder r h = 
    let sideArea = 2 * pi * r * h  
        topArea = pi * r ^2  
    in  sideArea + 2 * topArea 

countNonEmptyLines :: String -> Int
countNonEmptyLines inputString 
           = let 
              listOfLines = lines inputString
              trimmedList = filter (==" ") listOfLines

             in  length trimmedList

safeDiv :: Double -> Double -> Maybe Double
safeDiv a b 
 | b /= 0 = Just (a/b)
 | b == 0 = Nothing

 
maximum' :: (Ord a) => [a] -> a
maximum' [] = error "empty list"
maximum' [a]  = a
maximum' (x:xs)
    | x > maxTail = x
    | otherwise = maxTail
    where maxTail = maximum' xs

freqSort :: String -> String
freqSort xs = 
 let sorted = sort xs
     grouped = group sorted
     byLen = sortBy (comparing length) grouped
     result = concat byLen
 in result

curry1 :: ((a, b) -> c) -> a -> b -> c
curry1 f = \x y -> f (x, y)

uncurry1 :: (a -> b -> c) -> ((a, b) -> c)
uncurry1 f = \(x,y) -> f x y




upperFirst' :: String -> String
upperFirst' (c1:c2:rest) =
    if c1 == ' '  
        then c1 : toUpper c2 : upperFirst' rest
        else c1 : upperFirst' (c2:rest)
upperFirst' s = s

upperFirst :: String -> String
upperFirst [] = []
upperFirst [c] = [toUpper c]  -- No-op on non-letters
upperFirst (s:str) = upperFirst' (toUpper s:str)


upperBooks :: String -> String
upperBooks [] = []
upperBooks [a] = [toUpper a]
upperBooks (x:xs) = upperLong (toUpper x:xs)
                       where upperLong (x:y:z) =
                               if x == ' ' && y /= ' '
                                  then x : toUpper y : upperLong z
                                  else x : upperLong (y:z)
                             upperLong n = n


quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
      let smallSorted = quicksort [a | a <- xs, a <= x]
          biggerSorted = quicksort [a | a <- xs, a > x]
      in  smallSorted ++ [x] ++ biggerSorted


deleteLast :: Eq a => a -> [a] -> [a]
deleteLast _ [] = []
deleteLast n xs = helper [] [] xs
    where
        helper _ t [] = t
        helper initial deleted (x:xs) =
            if x == n then helper (initial ++ [x]) initial xs
            else helper (initial ++ [x]) (deleted ++ [x]) xs


winner results 
    | aScore <  bScore = ('B', bScore - aScore) 
    | aScore == bScore = ('-', 0)
    | aScore >  bScore = ('A', aScore - bScore)
   where
    aScore = length (filter (=='A') results)
    bScore = length (filter (=='B') results)
    

----- Constructing datatypes 1 ------


f1 :: (Int,Char,Bool) -> Char
f1 (a,b,c) = b

f2 :: (a,b,c) -> b
f2 (a,b,c) = b

f3 :: (a,(b,c,d),e) -> c
f3 (a,(b,c,d),e) = c

f4 :: [a] -> Maybe a
f4 [] = Nothing
f4 (x:xs) = Just x

f5 :: Either Int String -> Maybe String
f5 x = case  x of
      Left  x -> Just ""
      Right x -> Just x

f6 :: Either a b -> Maybe b
f6  x = case x of 
      Left  x  -> Nothing
      Right x  -> Just x 

g1 :: Maybe a -> b ->  Either a b
g1 (Just a) b = Left a
g1 Nothing b = Right b

g2 :: a -> b -> (a,b)
g2 x y = (x,y)


g3 :: a -> b ->  (Either a b)
g3 _ b =  Right b


area :: String -> Double -> Either String Double
area "circle" diameter = Right (pi * (diameter/2)**2)
area "square" diameter = Right (diameter / sqrt 2)
area _       _         = Left "Unknown shape"


----- Constructing datatypes 2 ------


newtype Meter = M Double 
    deriving (Eq,Ord,Show)

type Length = Int

data Vector2 a = V2 a a 
    deriving (Eq,Show)

data OneOrTwo a b = This a | That b | These a b
    deriving (Eq,Show)

data Submission 
  = S {student :: String, content :: String, date :: (Int,Int,Int)}
    deriving (Eq,Show)

asMeters :: Double -> Meter
asMeters x = M x

fromMeters :: Meter -> Double
fromMeters (M x) = x

asLength :: Int -> Length
asLength x = x

mkVector :: Int -> Int -> Vector2 Int
mkVector x y = V2 x y


vectorOut :: Vector2 Int -> Int
vectorOut x = case x of
                    V2 a b -> a

combine :: Vector2 Int -> Maybe (Vector2 Int) -> OneOrTwo Int Bool
combine x y = case y of
                  Nothing -> This (vectorOut x)
                  Just y -> These ((vectorOut x) * (vectorOut y)) False


combine3 :: Vector2 Int -> Maybe Bool -> Maybe String -> OneOrTwo Int (OneOrTwo Bool String)
combine3 x y z = case y of
                      Nothing -> case z of
                                      Nothing -> This (vectorOut x)
                                      Just z -> These (vectorOut x) (That z)
                      Just y -> case z of
                                      Nothing -> These (vectorOut x) (This y)
                                      Just z -> These (vectorOut x) (These y z)


submitDay :: Submission -> Int
submitDay (S {student = a, content = b, date = (c,d,e)}) = c

getOther :: OneOrTwo a b -> Maybe b 
getOther x = case x of
                This _ -> Nothing
                That m -> Just m
                These n m -> Just m





--fun :: [Double]
fun = let x = 3 in take x (repeat x)


--sad :: [Double]
sad = (\x -> take x (repeat x)) 3


kaka :: forall b. b -> b
kaka x = x

gaa :: forall a. a -> Bool
gaa x = True




---------------------------------


data ListOf a = Null | Cons a (ListOf a)

data PyType = PyBool Bool
            | PyInt Int
            | PyDouble Double
            | PyFun ([PyType] -> IO PyType)


-----------------------------------


fifa :: forall a. a -> a
fifa = \x -> x

--fufa :: forall a. a -> a
--fufa x = 5

figa :: forall a. [a] -> Maybe a
figa [] = Nothing
figa xs = Just (head . drop 5 . reverse $ xs)

foo :: forall a. a -> Bool
foo x = True

--------------------------------

{--
destutter [] = []                                          
destutter (x:xs) = x : (destutter (dropWhile (== x) xs))
--}

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
 

--------------------------------

swap :: [Integer] -> [Integer]
swap [] = []
swap [x] = [x]
swap (x1:x2:xs) = if x1 > x2 then (x2:x1: swap xs) else x1:x2:swap xs


--------------------------------

freqSort' :: String -> String
freqSort' a = concat . sortBy (comparing length) . group . sort $ a

--------------------------------



chain :: (Integral a) => a -> [a]  
chain 1 = [1]  
chain n  
    | even n =  n:chain (n `div` 2)  
    | odd n  =  n:chain (n*3 + 1)  

numLongChains :: Int  
numLongChains = length (filter isLong (map chain [1..100]))  
    where isLong xs = length xs > 15 





mum xs = sum' op empty xs
 where
  sum' o e (x:xs) = o x (sum' o e xs)
  sum' _ e []     = e

  op   a  b = a + b
  empty = 0



foldtest op empty (x:xs) = op x (foldtest op empty xs)
foldtest _  empty     [] = empty

sum''' xs = foldtest op empty xs
 where
  op    a b = a + b
  empty     = 0

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


drop'' :: Int -> [a] -> [a]
drop'' n xs = foldr count (const []) xs n
  where
    count x g 0 = []
    count x g n = x:g (n-1)






-----------------------------------



ack :: [Int] -> ([Int] -> [Int])
ack [ ] ys = 1 : ys
ack (x : xs) [ ] = ack xs [1]
ack (x : xs) (y : ys) = ack xs (ack (x : xs) ys)



---------------------------------

scanl'' :: (a -> b -> a) -> a -> [b] -> [a]
scanl'' f q xs = q : (case xs of
                            []      -> []
                            (x:xs)  -> scanl'' f (f q x) xs)

scanr'' :: (a -> b -> b) -> b -> [a] -> [b]
scanr'' f e []     = [e] 
scanr'' f e (x:xs) = let ys = scanr f e xs 
                     in f x (head ys) : ys 


------------------------



-- Take first n elements from a list
-- >>> take 3 "I am groot"
--     "I a"
{--take :: Int -> [a] -> [a] 
take n = g . foldr f e
    where 
      g :: (Int -> [a]) -> [a]
      g = undefined
      f :: a -> (Int -> [a]) -> (Int -> [a])
      f x acc =  \m -> undefined
      e = (const [])--}




-- Remove first n elements from a list
-- >>> drop 3 "m groot"
{--
-- drop :: Int -> [a] -> [a] 
drop n = g . foldr f e
    where 
      g = undefined
      f = undefined
      e = undefined
      --}
-- Hint: Write types for g, f and e first! 

-------------------------------

{-- both are wrong!

foldlTake :: Int -> [a] -> [a]
foldlTake = flip . foldl step $ const []
 where
    step :: (Int -> [a]) -> a -> (Int -> [a])
    step _    _    0 = []
    step next elem n = elem : next (n - 1)

foldlDrop :: Int -> [a] -> [a]
foldlDrop = flip . foldl step $ const []
 where
    step :: (Int -> [a]) -> a -> (Int -> [a])
    step next elem n@0 = elem : next n
    step next _    n   = next (n - 1) --}


removeFirst :: Eq a => a -> [a] -> [a] 
removeFirst rm = g . foldr f e
    where 
      g (x,y,isRemoved) = x
      f x (xs, ys, isRemoved)
        | (x /= rm)                       = ((x:xs),(x:ys),isRemoved)
        | (x == rm) && not (isRemoved)    = ((  xs),(x:ys),True)
        | otherwise                       = ((  ys),(x:ys),isRemoved)
      e = ([],[],False)


removeFirst' :: Eq a => a -> [a] -> [a] 
removeFirst' a xs = foldr f ini xs False
   where
    ini :: Bool -> [a]
    ini = const []
    f x acc = \isFound -> if isFound
      then x : acc True
      else if x == a then acc True 
                     else x : acc False


takeIt :: Int -> [a] -> [a]
takeIt n xs = foldr count (const []) xs n
  where
    count x g 0 = []
    count x g n = x:g (n-1)

{--
takeOp :: Int -> [a] -> [a] 
takeOp n = g . foldr f e
    where 
      g :: (Int -> [a]) -> [a]
      g (a, b) = b
      f :: a -> (Int -> [a]) -> (Int -> [a])
      f x acc =  \m -> 
        if m
          then x:acc 
          else if x==a 
            then acc True 
            else x:acc False
      e = (const [])--}


drop' :: Int -> [a] -> [a] 
drop' n = g . foldr f e
    where
      g :: (Int -> [a]) -> [a]
      g acc = acc (-n)
      f :: a -> (Int -> [a]) -> (Int -> [a])
      f x acc = \m  -> if m < 0 
         then acc (m + 1)
         else x : acc m
      e = (const [])
      
      
take' :: Int -> [a] -> [a] 
take' n = g . foldr f e
    where 
      g :: (Int -> [a]) -> [a]
      g acc = acc n
      f :: a -> (Int -> [a]) -> (Int -> [a])
      f x acc = \m -> if m > 0 
         then x : acc (m - 1)
         else const [] m
      e = (const [])





head'' :: [a] ->  Maybe a
head'' =  g . foldr f e
       where
        g x = x
        e = Nothing
        f = \x _ ->  Just x


fhead :: [a] -> Maybe a 
fhead = g . foldr f e
    where 
      g x = x
      f = \x _ -> Just x
      e = Nothing


tail' :: [a] -> [a]
tail' xs = foldr (\x xs (y:ys) -> ys) id xs xs



init' :: [a] -> Maybe [a]
init' = g . foldr f e
   where
    g x = x
    f _ Nothing   = Just []
    f x (Just xs) = Just (x:xs)
    e = Nothing


------------------------

 




{--
(<$>) :: (t -> ([a], t2)) -> ([a], t) -> ([a], t2)
f <$> x = case f _ of
    double _ -> (["Double"],x*2)
    increment x -> (["Incremented by 1"],x + 1)
    pure1 x     -> ([],x)
--}

{--

(<$>) :: (a -> b) -> Maybe a -> Maybe b
infixr 0 <$>
f <$> x = case x of
    Nothing -> Nothing
    Just value -> Just (f value)
--}

(.:::.) :: (b -> Maybe c) -> (a -> Maybe b) -> a -> Maybe c 
f .:::. g = \x -> case g x of
                 Nothing -> Nothing
                 Just y  -> f y



double    x = (["Double"],x*2)
increment x = (["Incremented by 1"],x + 1)
pure1      x = ([],x)

{--
experiment1 = let 
    (l1,step1) = pure1 5
    (l2,step2) = double step1
    (l3,step3) = double step2
    (l4,step4) = double step3
    (l5,step5) = increment step4
    (l6,step6) = increment step5
  in (l1++l2++l3++l4++l5++l6,step6)
--}

-- add some spice

(<$$>) :: (t -> ([a], t)) -> ([a], t) -> ([a], t)
infixr 0 <$$>

f  <$$>  (xs,x) = let 
            (ys,y) = f x
          in (xs++ys,y)

-- experiment1 = increment <$$> increment <$$> double <$$> (double <$$> double <$$> pure1 5)


(.::.) :: (t -> ([a], t)) -> (t -> ([a], t)) -> t -> ([a], t)

f .::. g = \x -> case g x of
                  (xs,x) -> let 
                       (ys,y) = f x
                        in (xs++ys,y)

experiment2 = (increment .::. increment .::. double .::. double .::. double .::. pure1) 5








-- Should return: (["Double","Double","Double","Incremented by 1","Incremented by 1"],42)

-- experiment2 = (increment . increment . double . double . double . pure1) 5
