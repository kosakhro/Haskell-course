
-- scanldinavians and scanrdinavians

scanl'' :: (a -> b -> a) -> a -> [b] -> [a]
scanl'' f q xs = q : (case xs of
                            []      -> []
                            (x:xs)  -> scanl'' f (f q x) xs)

scanr'' :: (a -> b -> b) -> b -> [a] -> [b]
scanr'' f e []     = [e] 
scanr'' f e (x:xs) = let ys = scanr f e xs 
                     in f x (head ys) : ys 
