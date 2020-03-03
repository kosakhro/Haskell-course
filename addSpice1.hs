

double    x = (["Double"],x*2)
increment x = (["Incremented by 1"],x + 1)
pure1      x = ([],x)

(<$$>) :: (t -> ([a], t2)) -> ([a], t) -> ([a], t2)
infixr 0 <$$>

f  <$$>  (l,t) = let 
            (l2,t2) = f t
            in (l++l2,t2)

experiment1 = increment <$$> increment <$$> double <$$> double <$$> double <$$> pure1 5
