data EncodeElem a = Single a | Multiple (Int, a) deriving (Show)

encodeModified :: Eq a => [a] -> [EncodeElem a]
encodeModified [] = []
encodeModified (x:xs) 
	| size == 1 = Single x : encodeModified xs
	| otherwise = Multiple (size, x) : encodeModified(dropWhile (x==) xs)
	where size = 1 + length(takeWhile (x==) xs)
