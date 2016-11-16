import Data.List (group, span)

-- 1: Find the last element of a list.
myLast :: [a] -> a
myLast [] = error "empty list."
myLast xs = xs !! (length xs - 1)

myLast' :: [a] -> a
myLast' [] = error "empty list."
myLast' xs = foldl1 (\acc x -> x) xs

myLast'' :: [a] -> a
myLast'' [] = error "empty list."
myLast'' xs = foldr1 (const id) xs

-- 2:Find the last but one element of a list.
myButLast :: [a] -> a
myButLast [] = error "empty list."
myButLast [x] = error "list has 1 element."
myButLast xs = xs !! (length xs - 2)

-- 3: Find the K'th element of a list. The first element in the list is number 1.
elementAt :: [a] -> Int -> a
elementAt xs i = xs !! (i-1)

-- 4: Find the number of elements of a list.
myLength :: Integral b => [a] -> b
myLength = foldl (\acc _ -> acc+1) 0

myLength' :: Integral b => [a] -> b
myLength' [] = 0
myLength' (x:xs) = 1 + myLength' xs

-- 5: Reverse a list.
myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x]

myReverse' :: [a] -> [a]
myReverse' = foldl (\l' x -> x:l') []

-- 6: Find out whether a list is a palindrome.
isPalindrome :: Eq a => [a] -> Bool
isPalindrome xs = xs == myReverse xs

-- 7: Flatten a nested list structure.
--    Transform a list, possibly holding lists as elements into a `flat' list by
--    replacing each list with its elements (recursively).
data NestedList a = Elem a | List [NestedList a]

flatten :: NestedList a -> [a]
flatten (Elem a) = [a]
flatten (List []) = []
flatten (List (x:xs)) = flatten x ++ flatten (List xs)

flatten' :: NestedList a -> [a]
flatten' (Elem a) = [a]
flatten' (List xs) = concatMap flatten xs

-- 8: Eliminate consecutive duplicates of list elements.
--    If a list contains repeated elements they should be replaced with a
--    single copy of the element. The order of the elements should not be
--    changed.
compress :: (Eq a) => [a] -> [a]
compress = (map head) . group

compress' :: (Eq a) => [a] -> [a]
compress' [] = []
compress' (x:xs) = x : (compress $ dropWhile (== x) xs)

compress'' :: (Eq a) => [a] -> [a]
compress'' (x:ys@(y:_))
    | x == y = compress'' ys
    | otherwise = x : compress'' ys
compress'' ys = ys

-------------------------------------------------------------------------------
compress''' xs = foldr f (const []) xs Nothing
    where f x r a@(Just q) | x == q = r a
          f x r _ = x : r (Just x)    -- que?
-------------------------------------------------------------------------------

-- 9: Pack consecutive duplicates of list elements into sublists. If a list
--    contains repeated elements they should be placed in separate sublists.
pack :: (Eq a) => [a] -> [[a]]
pack [] = []
pack l@(x:_) = let (xs, resto) = span (== x) l
               in xs : pack resto

-- 10: Run-length encoding of a list. Use the result of problem P09 to
--     implement the so-called run-length encoding data compression method.
--     Consecutive duplicates of elements are encoded as lists (N E) where N
--     is the number of duplicates of the element E.
encode :: (Eq a) => [a] -> [(Int, a)]
encode = map (\l -> (length l, head l)) . pack

encode' :: (Eq a) => [a] -> [(Int, a)]
encode' = map (\l -> (length l, head l)) . group

-- 11: Modified run-length encoding.
--
--     Modify the result of problem 10 in such a way that if an element has no
--     duplicates it is simply copied into the result list. Only elements with
--     duplicates are transferred as (N E) lists.
data ListItem a = Single a | Multiple Int a deriving (Show)
encodeModified :: (Eq a) => [a] -> [ListItem a]
encodeModified = map singleOrMultiple . pack
    where singleOrMultiple l = let largo = length l 
                               in if largo == 1
                                  then Single (head l)
                                  else Multiple largo (head l)

encodeModified' :: (Eq a) => [a] -> [ListItem a]
encodeModified' = map encodeHelper . encode
    where encodeHelper (1,e) = Single e
          encodeHelper (n,e) = Multiple n e

-- 12:




-- 31: Determine whether a given integer number is prime.
es_primo :: Integral a => a -> Bool
es_primo n = n > 1 && not (elem 0 [mod n m | m <- [2..raizEntera n]])
               where raizEntera = round . sqrt . fromIntegral

-- 32: Determine the greatest common divisor of two positive integer numbers.
--     Use Euclid's algorithm.
mcd :: Integral a => a -> a -> a
mcd a' b'
    | a == b = a
    | a > b  = mcd (a-b) b
    | a < b  = mcd a (b-a)
  where a = abs a'
        b = abs b'

-- 33: Determine whether two positive integer numbers are coprime. Two numbers
--     are coprime if their greatest common divisor equals 1.
coprimos :: Integral a => a -> a-> Bool
coprimos a b = mcd a b == 1

-- 34: Calculate Euler's totient function phi(m).

--     Euler's so-called totient function phi(m) is defined as the number of
--     positive integers r (1 <= r < m) that are coprime to m.

--     Example: m = 10: r = 1,3,7,9; thus phi(m) = 4. 
--     Note the special case: phi(1) = 1.
phi :: Integral a => a -> Int
phi 1 = 1
phi m = length [r | r <- [1..(m-1)], coprimos m r]

-- 40: Goldbach's conjecture.
goldbach n
    | n <= 2 = Left "ERROR: el numero debe ser mayor que 2."
    | mod n 2 /= 0 = Left "ERROR: el numero debe ser par."
    | otherwise = Right (a, b)
    where a = head (filter comp_es_primo (primos_menores n))
          b = n-a
          comp_es_primo x = es_primo (n-x)
          primos_menores n = filter es_primo [1..(div n 2)]