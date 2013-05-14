isPrime :: Integral a => a -> Bool
isPrime n
	| n <= 1 = False
	| n == 2 = True
	| even n = False
	| otherwise = all (/= 0) (map (mod n) [3,5..l])
		where l = ceiling (sqrt (fromIntegral n))

myGcd :: Int -> Int -> Int
myGcd a 0 = abs a
myGcd a b = myGcd b (mod a b)

coprime :: Int -> Int -> Bool
coprime a b = gcd a b == 1
