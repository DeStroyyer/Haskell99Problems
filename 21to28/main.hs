import Data.List

insertAt :: a -> [a] -> Int -> [a]
insertAt x xs n = take (n-1) xs ++ [x] ++ drop (n-1) xs

range :: Int -> Int -> [Int]
range m n
	| m > n = []
	| otherwise =  m : range (m+1) n

-- TODO PROBLEM 23 to 25 !

combinations :: Eq a => Int -> [a] -> [[a]]
combinations 0 _ = [[]]
combinations _ [] = []
combinations n (x:xs) = map (x:) (combinations (n-1) xs) ++ combinations n xs
