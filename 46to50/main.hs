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

tableList :: (Bool -> Bool -> Bool) -> [(Bool, Bool, Bool)]
tableList f = [(a, b, f a b) | a <- values, b <- values]
	where values = [True, False]

showTable :: [(Bool, Bool, Bool)] -> IO ()
showTable [] = return ()
showTable ((a,b,c):xs) = putStrLn (show a ++ " " ++ show b ++ " " ++ show c) >> showTable xs

table :: (Bool -> Bool -> Bool) -> IO ()
table = showTable . tableList
