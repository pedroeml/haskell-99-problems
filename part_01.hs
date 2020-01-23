-- Problem 1
myLast :: [a] -> a
myLast [x] = x
myLast (x:xs) = myLast xs

-- Problem 2
myButLast :: [a] -> a
myButLast (x:xs)
    | length (x:xs) == 2 = x
    | length (x:xs) > 2 = myButLast xs

-- Problem 3
elementAt :: [a] -> Int -> a
elementAt (x:xs) 1 = x
elementAt (x:xs) k
    | length (x:xs) >= k = elementAt xs (k - 1)

-- Problem 4
myLength :: [a] -> Int
myLength [] = 0
myLength (x:xs) = (myLength xs) + 1 

-- Problem 5
myReverse :: [a] -> [a]
myReverse [] = []
myReverse [x] = [x]
myReverse (x:xs) = (myReverse xs) ++ [x]

-- Problem 6
isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome xs = xs == (myReverse xs)

-- Problem 7
myFlatten :: [[a]] -> [a]
myFlatten [] = []
myFlatten (x:xs) = x ++ (myFlatten xs)

-- Problem 8
myCompress :: (Eq a) => [a] -> [a]
myCompress [] = []
myCompress [x] = [x]
myCompress (x:xs)
    | x == (head xs) = myCompress xs
    | otherwise = [x] ++ (myCompress xs)

-- Problem 9
myPack :: (Eq a) => [a] -> [[a]]
myPack [] = []
myPack [x] = [[x]]
myPack (x:xs) = myPackAux (x:xs) [] []
    where
        myPackAux :: (Eq a) => [a] -> [a] -> [[a]] -> [[a]]
        myPackAux [] [] zs = zs
        myPackAux [] ys zs = zs ++ [ys]
        myPackAux (x:xs) [] zs = myPackAux xs [x] zs
        myPackAux (x:xs) ys zs
            | x == (head ys) = myPackAux xs (x:ys) zs
            | otherwise = myPackAux (x:xs) [] (zs ++ [ys])

-- Problem 10
myEncode :: (Eq a) => [a] -> [(Int, a)]
myEncode [] = []
myEncode [x] = [(1, x)]
myEncode (x:xs) = myEncodeAux (myPack (x:xs))
    where
        myEncodeAux :: (Eq a) => [[a]] -> [(Int, a)]
        myEncodeAux [] = []
        myEncodeAux [x] = [((length x), (head x))]
        myEncodeAux (x:xs) = (myEncodeAux [x]) ++ (myEncodeAux xs)
