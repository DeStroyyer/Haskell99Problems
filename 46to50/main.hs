infixl 4 `or'`
infixl 4 `nor'`
infixl 5 `xor'`
infixl 6 `and'`
infixl 6 `nand'`
infixl 3 `equ'`

and' :: Bool -> Bool -> Bool
and' True True = True
and' _ _ = False

or' :: Bool -> Bool -> Bool
or' False False = False
or' _ _ = True

nand' :: Bool -> Bool -> Bool
nand' a b = not (and' a b)

nor' :: Bool -> Bool -> Bool
nor' a b = not (or' a b)

xor' :: Bool -> Bool -> Bool
xor' a b = a /= b

impl' :: Bool -> Bool -> Bool
impl' a b = a && b || not a

equ' :: Bool -> Bool -> Bool
equ' = (==)

-- problem 46/47
table :: (Bool -> Bool -> Bool) -> IO ()
table = showTable . tableList

tableList :: (Bool -> Bool -> Bool) -> [(Bool, Bool, Bool)]
tableList f = [(a, b, f a b) | a <- values, b <- values]
	where values = [True, False]

showTable :: [(Bool, Bool, Bool)] -> IO ()
showTable [] = return ()
showTable ((a,b,c):xs) = putStrLn (show a ++ " " ++ show b ++ " " ++ show c) >> showTable xs

-- problem 48
tablen :: Int -> ([Bool] -> Bool) -> IO ()
tablen n f = showTablen [xs ++ [f xs] | xs <- combos n]

combos :: Int -> [[Bool]]
combos n
	| n == 1 = singles
	| n == 2 = combos' singles
	| otherwise = combos' (combos (n - 1))
	where 
	singles = [[True], [False]]
	combos' xs = [a : b | a <- [True, False], b <- xs]

showTablen :: [[Bool]] -> IO ()
showTablen [] = return ()
showTablen (x:xs) = showLine x >> showTablen xs

showLine :: [Bool] -> IO ()
showLine [] = putStrLn ""
showLine (x:xs) = putStr (show x ++ space) >> showLine xs
	where space | x == True = "  "
		    | otherwise = " "

-- problem 49
