import Data.Char

type StudentMark = (String, Int)

betterStudent :: StudentMark -> StudentMark -> String
betterStudent (s1,m1) (s2,m2) 
    | m1 >= m2          = s1
    | otherwise         = s2

marks:: [StudentMark] -> [Int]
marks stmks = [ mk | (st,mk) <- stmks ]

pass :: [StudentMark] -> [String]
pass stmks = [ st | (st,mk) <- stmks, mk >= 40 ]

-- An example list of student marks
testData :: [StudentMark]
testData = [("John", 53), ("Sam", 16), ("Kate", 85), ("Jill", 65),
            ("Bill", 37), ("Amy", 22), ("Jack", 41), ("Sue", 71)]

addPairs :: [(Int,Int)] -> [Int]
addPairs pairList = [ i+j | (i,j) <- pairList ]

minAndMax :: Int -> Int -> (Int,Int)
minAndMax x y 
    | x <= y            = (x,y)
    | otherwise         = (y,x)

sumDifference :: Int -> Int -> (Int,Int)
sumDifference a b = (a + b, abs(a - b))



grade :: StudentMark -> Char
grade (st,mk)
    | mk >= 70 = 'A'
    | mk  >= 60 = 'B'
    | mk  >= 50 = 'C'
    | mk >= 40 = 'D'
    | otherwise = 'F'

capMark :: StudentMark -> StudentMark
capMark (st,mk)
    | mk < 40 = (st,mk)   
    | otherwise = (st,40)


firstNumbers :: Int -> [Int]
firstNumbers n = [1..n]

firstSquares :: Int -> [Int]
firstSquares n = [x ^ 2 | x <- firstNumbers(n)]

capitalise :: String -> String
capitalise text = [toUpper c | c <- text]

onlyDigits :: String -> String
onlyDigits text = [ c | c <- text, isDigit c ]

capMarks :: [StudentMark] -> [StudentMark]
capMarks stMk = [capMark (st,mk) | (st,mk) <- stMk]

gradeStudents :: [StudentMark] -> [(String,Char)]
gradeStudents stMk = [ (st, grade (st,mk)) | (st,mk) <- stMk ]


duplicate:: String -> Int -> String
duplicate text n 
    | n > 1 = text ++ duplicate text (n - 1)
    | otherwise = text

