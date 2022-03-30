headPlusOne :: [Int] -> Int
headPlusOne [] = 0
headPlusOne  (x:xs) = x + 1

duplicateHead :: [a] -> [a]
duplicateHead [] = []
duplicateHead (x:xs) = (x:(x:xs))

rotate :: [a] -> [a]
rotate [] = []
rotate[x] = [x]
rotate (x:y:xs) = (y:x:xs)

listLength :: [a] -> Int 
listLength [] = 0
listLength (x:xs) = 1 + listLength (xs)

multAll :: [Int] -> Int
multAll [] = 1
multAll (x:xs) = x * multAll xs

andAll :: [Bool] -> Bool
andAll [] = True
andAll (True:xs) = True == andAll(xs)
andAll (False:_) = False

countElems :: Int -> [Int] -> Int
countElems n (x:xs)
    | listLength (x:xs) == 1 && n == x = 1
    | listLength (x:xs) == 1 = 0
    | n == x = 1 + countElems n (xs)
    | n /= x = countElems n (xs)
    |otherwise = 0

removeAll :: Int -> [Int] -> [Int]
removeAll n (x:xs)
    | listLength (x:xs) == 1 && n == x = []
    | listLength (x:xs) == 1 = [x]
    | n /= x = [x] ++ removeAll n xs 
    | n == x = removeAll n xs