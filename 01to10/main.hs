-- problem 1
myLast :: [a] -> a
myLast [x] = x
myLast (_:xs) = myLast xs

-- problem 2
myButLast :: [a] -> a
myButLast [x, _] = x
myButLast (_:xs) = myButLast xs

-- problem 3
elementAt :: [a] -> Int -> a
elementAt xs n = xs !! (n-1)

-- problem 4
myLength :: [a] -> Int
myLength [] = 0
myLength (_:xs) = 1 + myLength xs

-- problem 5
myReverse :: [a] -> [a]
myReverse [x] = [x]
myReverse (x:xs) = myReverse(xs) ++ [x]

-- problem 6
isPalindrome :: Eq a => [a] -> Bool
isPalindrome [] = True
isPalindrome [_] = True
isPalindrome (x:xs) 
	| x /= myLast xs = False
	| otherwise 	 = isPalindrome(dropLast xs)

dropLast :: [a] -> [a]
dropLast xs = take (myLength(xs) - 1) xs

-- problem 7
data NestedList a = Elem a | List [NestedList a]

flatten :: NestedList a -> [a]
flatten l = case l of
	Elem e -> [e]
	List ls -> concat(map flatten ls)

-- problem 8
compress :: Eq a => [a] -> [a]
compress [] = []
compress (x:xs) = x : compress(dropWhile (x==) xs)

-- problem 9
pack :: Eq a => [a] -> [[a]]
pack [] = []
pack (x:xs) =  (x:takeWhile (x==) xs) : pack(dropWhile (x==) xs)

-- problem 10
encode :: Eq a => [a] -> [(Int, a)]
encode [] = []
encode (x:xs) = (1+myLength(takeWhile (x==) xs), x) : encode(dropWhile (x==) xs)
