{-
Tatyusa solutions for H-99: Ninety-Nine Haskell Problems
http://www.haskell.org/haskellwiki/H-99:_Ninety-Nine_Haskell_Problems
-}

System.Random

-- Problem 1
myLast :: [a] -> a
myLast [x]    = x
myLast (_:xs) = myLast xs

-- Problem 2
myButLast :: [a] -> a
myButLast [x,_] = x
myButLast (_:xs) = myButLast xs

-- Problem 3
elementAt :: [a] -> Int -> a
elementAt (x:xs) 1 = x
elementAt (x:xs) n = elementAt xs (n-1)

-- Problem 4
myLength :: [a] -> Int
myLength []     = 0
myLength (_:xs) = 1 + myLength xs

-- Problem 5
myReverse :: [a] -> [a]
myReverse []     = []
myReverse (x:xs) = (myReverse xs) ++ [x]

-- Problem 6
isPalindrome :: Eq a => [a] -> Bool
isPalindrome l = l == (myReverse l)

-- Problem 7
data NestedList a = Elem a | List [NestedList a]
flatten :: NestedList a -> [a]
flatten (Elem x) = [x]
flatten (List l) = foldl (++) [] (map flatten l)

-- Problem 8
compress :: Eq a => [a] -> [a]
compress []     = []
compress (x:xs) = reverse $ foldl (\l y -> if head l == y then l else y:l) [x] xs

-- Problem 9
pack :: Eq a => [a] -> [[a]]
pack []     = []
pack all@(x:_) = (takeWhile (==x) all) : (pack $ dropWhile (==x) all)

-- Problem 10
encode :: Eq a => [a] -> [(Int,a)]
encode l = map (\wds -> (length wds, head wds)) $ pack l

-- Problem 11
data Composition a = Multiple Int a | Single a deriving (Show)
encodeModified :: Eq a => [a] -> [Composition a]
encodeModified [] = []
encodeModified l  = map (\wds -> if length wds == 1 then Single (head wds) else Multiple (length wds) (head wds)) $ pack l

-- Problem 12
decodeModified :: [Composition a] -> [a]
decodeModified []     = []
decodeModified (Single x:xs)     = [x] ++ decodeModified xs
decodeModified (Multiple n x:xs) = (take n $ repeat x) ++ (decodeModified xs)

-- Problem 13
encodeDirect :: Eq a => [a] -> [Composition a]
encodeDirect []         = []
encodeDirect all@(x:xs) = makeComp (takeWhile (==x) all) : (encodeDirect $ dropWhile (==x) xs)
  where
    makeComp :: [a] -> Composition a
    makeComp [x]        = Single x
    makeComp all@(x:xs) = Multiple (length all) x

-- Problem 14
dupli :: [a] -> [a]
dupli []     = []
dupli (x:xs) = x : x : (dupli xs)

-- Problem 15
repli :: [a] -> Int -> [a]
repli [] _     = []
repli (x:xs) n = (take n $ repeat x) ++ (repli xs n)

-- Problem 16
dropEvery :: [a] -> Int -> [a]
dropEvery [] _  = []
dropEvery [x] 1 = []
dropEvery [x] _ = [x]
dropEvery xs n  = init (take n xs) ++ (dropEvery (drop n xs) n)

-- Problem 17
split :: [a] -> Int -> [[a]]
split xs n = [take n xs] ++ [drop n xs]

-- Problem 18
slice :: [a] -> Int -> Int -> [a]
slice xs n m = drop (n-1) (take m xs)

-- Problem 19
rotate :: [a] -> Int -> [a]
rotate xs n
  | n == 0    = xs
  | n < 0     = rotate ([last xs] ++ (init xs)) (n+1)
  | otherwise = rotate (tail xs ++ [head xs]) (n-1)

-- Problem 20
removeAt :: Int -> [a] -> (a, [a])
removeAt n xs = ( xs!!(n-1), (take (n-1) xs)++(drop n xs) )

-- Problem 21
insertAt :: a -> [a] -> Int -> [a]
insertAt x xs n = (take (n-1) xs) ++ [x] ++ (drop (n-1) xs)

-- Problem 22
range :: (Eq a, Enum a) => a -> a -> [a]
range n m
  | n == m    = [n]
  | otherwise = n : range (succ n) m

-- Problem 23
rnd_select :: [a] -> Int -> IO [a]
rnd_select xs n = do
  
