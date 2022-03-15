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

listLength :: [] -> int 
listLength [] = 0
listLength (x:xs) = 1 + listLength (xs)