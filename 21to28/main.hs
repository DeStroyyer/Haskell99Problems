import Data.List
import System.Random

-- problem 21
insertAt :: a -> [a] -> Int -> [a]
insertAt x xs n = take (n-1) xs ++ [x] ++ drop (n-1) xs

-- problem 22
range :: Int -> Int -> [Int]
range m n
	| m > n = []
	| otherwise =  m : range (m+1) n

-- problem 23
rndSelect :: Eq a => [a] -> Int -> IO [a]
rndSelect _ 0 = return []
rndSelect xs n = do x <- rndSelectElem xs;
		    y <- rndSelect (rmElem xs x) (n-1)
		    return (x:y)

rndSelectElem :: [a] -> IO a
rndSelectElem xs = randomRIO (0, length xs - 1) >>= \i -> return (xs !! i)

rmElem :: Eq a => [a] -> a -> [a]
rmElem [] _ = []
rmElem (x:xs) e
	| x == e = xs
	| otherwise = x : rmElem xs e

-- TODO problem 24 - 25

-- problem 26
combinations :: Eq a => Int -> [a] -> [[a]]
combinations 0 _ = [[]]
combinations _ [] = []
combinations n (x:xs) = map (x:) (combinations (n-1) xs) ++ combinations n xs

-- TODO problem 27 - 28
