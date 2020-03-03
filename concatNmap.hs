a. concat (map f x:map (map f) xs) == map f x ++ concat (map (map f) xs)

using this concat function definition :

(1) concat (x:xs) = x ++ concat xs

concat (map f x:map (map f) xs)
{using (1)}
== map f x ++ concat (map (map f) xs)




b. map f x ++ map f y == map f (x++y) for all lists x, y 

using those definitions:

(1) map f [] = []
(2) map f (x:xs) = f x : map f xs
(3) [] ++ ys = ys
(4) (x:xs) ++ ys = x : (xs ++ ys)

-- case x == []
map f [] ++ map f y 
{using (1)}
== [] ++ map fy
{using (3)}
== map f y 
{using (3) and assumption}
== map f ([] ++ y) 

-- case x is not empty
map f x ++ map f y 
{using x definition}
== map f (x:xs) ++ map f y
{using (2)}
== (f x : f xs) ++ map f y
{using (4)}
== f x : (map f xs ++ map f y)
{using (4)}
== f x : map f (xs ++ y)
{using (2)}
== map f (x : (xs ++ y))
{using (4)}
== map f ((x:xs) ++ y)
{using x definition}
== map f (x ++ y)



