type StudentMark = (String, Int)

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
andAll (x:xs) = x && andAll xs

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
    -- | n /= x = [x] ++ removeAll n xs 
    | n /= x = x: removeAll n xs 
    | otherwise= removeAll n xs

listMarks :: String -> [StudentMark] -> [Int]
listMarks name stmks = [ mk | (st,mk) <- stmks , name == st ]

sorted :: [Int] -> Bool
sorted (x:y:xs)
    | xs == [] && x <= y = True
    | x <= y = True == sorted (y:xs)
    |otherwise = False

prefix :: [Int] -> [Int] -> Bool
prefix [] _ = True
prefix _ [] = False
prefix (a:as) (b:bs) = (a==b) && prefix as bs
    -- | a == b = prefix as bs
    -- | otherwise = False

subSequence :: [Int] -> [Int] -> Bool
subSequence [] _ = True
subSequence _ [] = False
subSequence l (b:bs)
    | prefix l (b:bs) = True
    | otherwise = subSequence l bs
