import Data.List

-- problem 21
insertAt :: a -> [a] -> Int -> [a]
insertAt x xs n = take (n-1) xs ++ [x] ++ drop (n-1) xs

-- problem 22
range :: Int -> Int -> [Int]
range m n
	| m > n = []
	| otherwise =  m : range (m+1) n

-- TODO problem 23 to 25 !

-- problem 26
combinations :: Eq a => Int -> [a] -> [[a]]
combinations 0 _ = [[]]
combinations _ [] = []
combinations n (x:xs) = map (x:) (combinations (n-1) xs) ++ combinations n xs

-- TODO problem 27 - 28
