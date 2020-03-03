
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


-- Now, demonstrate that you can construct the above datums by implementing 
-- the following functions:


asMeters :: Double -> Meter
asMeters x = M x

fromMeters :: Meter -> Double
fromMeters (M x) = x

asLength :: Int -> Length
asLength x = x

mkVector :: Int -> Int -> Vector2 Int
mkVector x y = V2 x y

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

-- It is not really important what the functions do precisely, 
-- but they need to have the correct type, and they must be total 
-- (that is, you must handle all possible cases).

