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

totient :: Int -> Int
totient m = length (filter (coprime m) [1..(m-1)])

primeFactors :: Int -> [Int]
primeFactors n = factorize n primes
	where primes = filter (isPrime) [1..n]

factorize :: Int -> [Int] -> [Int]
factorize _ [] = []
factorize n (x:xs)
	| n <= 1 = []
	| n `mod` x == 0 = x : factorize (quot n x) (x:xs)
	| otherwise = factorize n xs

primeFactorsMult :: Int -> [(Int, Int)]
primeFactorsMult = pairMult . primeFactors

pairMult :: [Int] -> [(Int, Int)]
pairMult [] = []
pairMult (x:xs) = (x, len) : pairMult (dropWhile (==x) xs)
	where len = length (takeWhile (==x) (x:xs))

phi :: Int -> Int
phi n = mult (map (\(p, m) -> (p-1) * p ^ (m-1)) factors)
	where factors = primeFactorsMult n

mult :: [Int] -> Int
mult = foldr (*) 1

primesR :: Int -> Int -> [Int]
primesR a b = filter isPrime [a..b]
