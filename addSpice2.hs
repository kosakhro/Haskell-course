
(.::.) :: (t -> ([a], t)) -> (t -> ([a], t)) -> t -> ([a], t)

f .::. g = \x -> case g x of
                  (xs,x) -> let 
                       (ys,y) = f x
                        in (xs++ys,y)

experiment2 = (increment .::. increment .::. double .::. double .::. double .::. pure1) 5