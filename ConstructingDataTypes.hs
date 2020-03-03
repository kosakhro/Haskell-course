
f1 :: (Int,Char,Bool) -> Char
f1 (a,b,c) = b

f2 :: (a,b,c) -> b
f2 (a,b,c) = b

f3 :: (a,(b,c,d),e) -> c
f3 (a,(b,c,d),e) = c

f4 :: [a] -> Maybe a
f4 [] = Nothing
f4 (x:xs) = Just x

f5 :: Either Int String -> String
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