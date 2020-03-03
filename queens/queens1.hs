type Queen = (Int,Int)
type Setup = [Queen]

showBoard :: Setup -> Table StaticPicture
showBoard queens = T [[mark (x,y) 
                     | x <- [1 .. 8]] 
                     | y <- [1 .. 8]]
 where
  mark pt
   | any (threatens pt) queens 
     && pt `elem` queens      
    = Pic ("/images/TQueen.png")  -- A threatened queen
   | pt `elem` queens          
    = Pic ("/images/Queen.png")   -- A queen
   | any (threatens pt) queens 
    = Pic ("/images/TSquare.png") -- A threatened square
   | otherwise                 
    = Pic ("/images/Square.png")  -- An empty square

threatens :: Queen -> Queen -> Bool
threatens (a1,a2) (b1,b2)
   | (a1,a2) == (b1,b2) = False -- Queen doesn't threaten herself
   | a2 == b2           = True  -- On the same row
   | a1 == b1           = True  -- On the same column
   | (a1-a2) == (b1-b2) = True  -- diagonal
   | (a1+a2) == (b1+b2) = True  -- diagonal
   | otherwise = False



addQueen :: Setup -> [Setup]
addQueen qs = [new:qs 
              | y <- [1 .. 8] 
              , let new = (1 + length qs,y) 
              , not (any (threatens new) qs) 
              ]


queens :: Setup -> [Setup]
queens qs | length qs >= 8  = [qs]
          | otherwise       = concat (map queens (addQueen qs))

