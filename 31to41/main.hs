isPrime :: Integral a => a -> Bool
isPrime n
	| n <= 1 = False
	| n == 2 = True
	| even n = False
	| otherwise = all (/= 0) (map (mod n) [3,5..l])
		where l = ceiling (sqrt (fromIntegral n))

myGcd :: Int -> Int -> Int
myGcd a 0 = a
myGcd a b = abs (myGcd b (mod a b))
