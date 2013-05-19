data EncodeElem a = Single a | Multiple (Int, a) deriving (Show)

-- problem 11/13
encodeModified :: Eq a => [a] -> [EncodeElem a]
encodeModified [] = []
encodeModified (x:xs) 
	| size == 1 = Single x : encodeModified xs
	| otherwise = Multiple (size, x) : encodeModified(dropWhile (x==) xs)
	where size = 1 + length(takeWhile (x==) xs)

-- problem 12
decodeModified :: [EncodeElem a] -> [a]
decodeModified [] = []
decodeModified (x:xs) = case x of
	Single s -> s : decodeModified xs
	Multiple (n, m) ->  replicate n m ++ decodeModified xs

-- problem 14
dupli :: [a] -> [a]
dupli xs = repli xs 2

-- problem 15
repli :: [a] -> Int -> [a]
repli xs n = concat(map (replicate n) xs)

-- problem 16
dropEvery :: [a] -> Int -> [a]
dropEvery xs n = dropEveryHelp 1 xs n

dropEveryHelp :: Int -> [a] -> Int -> [a]
dropEveryHelp _ [] _ = []
dropEveryHelp i (x:xs) n
	| i == n = dropEveryHelp 1 xs n
	| otherwise = x : dropEveryHelp (i+1) xs n

-- problem 17
split :: [a] -> Int -> [[a]]
split xs n = [take' n xs, drop' n xs]

take' :: Int -> [a] -> [a]
take' _ [] = []
take' n (x:xs)
	| n > 0 = x : take' (n-1) xs
	| otherwise = []

drop' :: Int -> [a] -> [a]
drop' _ [] = []
drop' n (x:xs)
	| n <= 1 = xs
	| otherwise = drop' (n-1) xs

-- problem 18
slice :: [a] -> Int -> Int -> [a]
slice xs i j = take (j-i+1) (drop (i-1) xs)

-- problem 19
rotate :: [a] -> Int -> [a]
rotate xs n
	| n >= 0 = drop n xs ++ take n xs
	| otherwise = drop (length(xs) + n) xs ++ take (length(xs) + n) xs

-- problem 20
removeAt :: Int -> [a] -> (a, [a])
removeAt n (x:xs)
	| n == 1 = (x, xs)
	| otherwise = (fst rec, x : snd rec)
		where rec = removeAt (n-1) xs

