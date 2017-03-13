-------------------------------------------------------------------------------------------
--- Question 1 :

--Method 1:
slice::Int->Int->[Int]->[Int]
slicetemp::Int->Int->Int->[Int]->[Int]
slice _ _ [] = []
slice i j (x:xs) = slicetemp i j 1 (x:xs)
slicetemp _ _ _ [] = []
slicetemp i j k (x:xs)
	| (k>=i && k<=j )= [x] ++ slicetemp i j (k+1) xs
	| otherwise = slicetemp i j (k+1) xs



-- Method 2:
--slice :: [Int] -> Int -> Int -> [Int]
--slice [] _ _  = []
--slice (x:xs) i k
-- | i > 1      = slice xs (i - 1) (k - 1)
-- | k < 1      = []
--  | otherwise  = x ++ slice xs (i - 1) (k - 1)
--slice i j (x:xs) = [x|x<-(xs),(getindex x (x:xs) )>=i,(getindex x (x:xs) )<=j]

--getindex::Int -> [Int]->Int
--indtemp ::Int -> Int -> [Int]->Int
--getindex _ [] = error "Search cannot be initated as List is empty"
--getindex n (x:xs) = indtemp n 1 (x:xs) 
--indtemp _ _ [] = error "not found"
--indtemp n i (x:xs)	
--	| n==x = i
--	| otherwise = indtemp n (i+1) xs



-- Method 3
--findn:: [Int]->Int->[Int]
--findn(x:_)1=[x]
--findn(_:xs)p=findn xs(p-1)

--append::[Int]->[Int]->[Int]
--append []r=r
--append (x:xs)r=x:append xs r

--slice :: Int->Int->[Int]->[Int]
--slice p q(xs)
--                |q==p+1 = append (findn xs p) (findn xs q)
--                |otherwise=findn xs p ++ slice (p+1) q xs


-------------------------------------------------------------------------------------------
--- Question 2 :

deleteInside::[Int] ->  Int->Int ->[Int]
delete :: Int->[Int]->[Int]

delete _ [] = []
delete input (x:xs) = deleteInside (x:xs) input 1 
deleteInside [] _ _ = [] 
deleteInside (x:xs) input index 
   		| input==0 = x:xs
   		| input<0 = error "Negative Value cannot be Computed"
   		| (index `mod` input) == 0   = []  ++ (deleteInside xs input (index+1) )
   		| (index `mod` input) /= 0   = [x]  ++ (deleteInside xs input (index+1) )


------------------------------------------------------------------------------------------
--- Question 3 :

data Tree=Leaf Int|Node Tree Int Tree
occurs:: Int->Tree->Bool

occurs input (Leaf node)
	|node==input	=True
	|otherwise	=False
occurs input (Node tree_left next_node tree_right)
	|next_node==input	=True
	|input<next_node	=occurs input tree_left
	|input>next_node	=occurs input tree_right


-------------------------------------------------------------------------------------------
--- Question 4 :

-- Using list Comprehension :
-- sortlist :: [[Int]] -> [[Int]]
-- sortlist [] = []
-- sortlist (x:xs) = sortlist [y| y<-xs, length y<=length x]
-- 				 ++[x]++ 
-- 				 sortlist [y| y <- xs , length y >length x]


--- Using List :
sortlist :: [[Int]] -> [[Int]]
sortlist [x] = [x]
sortlist (x:xs) = sort x (sortlist xs)

sort :: [Int] -> [[Int]] -> [[Int]]
sort x [] = [x]
sort x (y:ys) = if length x < length y 
                 then x:y:ys 
         else y : sort x ys

-------------------------------------------------------------------------------------------
--- Question 5 :

-- Method 1:
flatten:: [[Int]]->[Int]
flatten []=[]
flatten([]:xs)=flatten (xs)
flatten ((x:xs):ys) =x:flatten(xs:ys)

-- Method 2:
--flatten:: [[Int]]->[Int]
---latten []=[]
--flatten(x:xs)=x ++ flatten xs
-------------------------------------------------------------------------------------------
