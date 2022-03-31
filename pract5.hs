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
countElems n [] = 0
countElems n (x:xs)
    | n == x = 1 + countElems n (xs)
    | n /= x = countElems n (xs)
  
removeAll :: Int -> [Int] -> [Int]
removeAll n [] = []
removeAll n (x:xs)
    | n /= x = [x] ++ removeAll n xs
    |otherwise = removeAll n xs

listMarks :: String -> [StudentMark] -> [Int]
listMarks name stmks = [ mk | (st,mk) <- stmks , name == st ]

sorted :: [Int] -> Bool
sorted [] = True
sorted (x:y:[]) = x <= y
sorted (x:y:xs)
    | x <= y = sorted (y:xs)
    |otherwise = False

prefix :: [Int] -> [Int] -> Bool
prefix [] _ = True
prefix _ [] = False
prefix (a:as) (b:bs) = (a==b) && prefix as bs

subSequence :: [Int] -> [Int] -> Bool
subSequence [] _ = True
subSequence _ [] = False
subSequence a (b:bs)
    | prefix a (b:bs) = True
    | otherwise = subSequence a bs
