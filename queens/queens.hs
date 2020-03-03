

queens :: Int -> [[Int]]
queens n 
   | n == 1             = [[1]] -- 1 queen has 1 solution
   | n == 8   = merges . merges . merges $ [[x] | x <- [1..n]]   -- making 1-column solutions for n queens 
   | otherwise          = error "Use only 2^n number of queens due to algorithm's restrictions\n more tnan 8 can't calculate due to inefficiency "
        where merges (x:xs)
                 | length [[x]]  >= n = (x:xs)          
                 | otherwise          =  [a++b | a <- (x:xs) , b <- (x:xs), safe (a++b)]
                           

-- True if no threats found 
safe :: [Int] -> Bool
safe x = diagSafe x && allDiff x
   where allDiff list = case list of        -- checking all horisontal threats
             []      -> True
             (x:xs)  -> x `notElem` xs && allDiff xs
         diagSafe list = case list of       -- checking all diagonal threats
             []     -> True
             (x:xs) -> ( and [abs (x - qi) /= i | (qi,i) <- xs `zip` [1..]]) && (diagSafe xs)

