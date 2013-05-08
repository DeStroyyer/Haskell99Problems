myLast :: [a] -> a
myLast [x] = x
myLast (_:xs) = myLast xs

myButLast :: [a] -> a
myButLast [x, _] = x
myButLast (_:xs) = myButLast xs

elementAt :: [a] -> Int -> a
elementAt xs n = xs !! (n-1)

myLength :: [a] -> Int
myLength [] = 0
myLength (_:xs) = 1 + myLength xs

myReverse :: [a] -> [a]
myReverse [x] = [x]
myReverse (x:xs) = myReverse(xs) ++ [x]

isPalindrome :: Eq a => [a] -> Bool
isPalindrome [] = True
isPalindrome [_] = True
isPalindrome (x:xs) 
	| x /= myLast xs = False
	| otherwise 	 = isPalindrome(dropLast xs)

dropLast :: [a] -> [a]
dropLast xs = take (myLength(xs) - 1) xs

data NestedList a = Elem a | List [NestedList a]

flatten :: NestedList a -> [a]
flatten l = case l of
	Elem e -> [e]
	List ls -> concat(map flatten ls)

compress :: Eq a => [a] -> [a]
compress [] = []
compress (x:xs) = x : compress(dropWhile (x==) xs)

pack :: Eq a => [a] -> [[a]]
pack [] = []
pack (x:xs) =  (x:takeWhile (x==) xs) : pack(dropWhile (x==) xs)

