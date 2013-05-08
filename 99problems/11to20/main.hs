data EncodeElem a = Single a | Multiple (Int, a) deriving (Show)

encodeModified :: Eq a => [a] -> [EncodeElem a]
encodeModified [] = []
encodeModified (x:xs) 
	| size == 1 = Single x : encodeModified xs
	| otherwise = Multiple (size, x) : encodeModified(dropWhile (x==) xs)
	where size = 1 + length(takeWhile (x==) xs)

decodeModified :: [EncodeElem a] -> [a]
decodeModified [] = []
decodeModified (x:xs) = case x of
	Single s -> s : decodeModified xs
	Multiple (n, m) ->  replicate n m ++ decodeModified xs

dupli :: [a] -> [a]
dupli xs = repli xs 2

repli :: [a] -> Int -> [a]
repli xs n = concat(map (replicate n) xs)
