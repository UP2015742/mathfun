-- We don't import '||' from the prelude, so that we can 
-- define our own version

import Prelude hiding ((||),(&&),gcd) 

-- The following line declares the || operator (which we are about to
-- re-define) to be right associative and to have precedence 2. This
-- is necessary in order for expressions such as False || x > 2 to be
-- valid (e.g. it sets the precedence of || to be lower than >). 

infixr 2  ||

-- A naive re-implementation of the Prelude operator ||
(||) :: Bool -> Bool -> Bool
True || True    = True
False || True   = True
True || False   = True
False || False  = False

-- An alternative re-implementation
--(||) :: Bool -> Bool -> Bool
--False || False   = False
--_ || _           = True

-- Another alternative re-implementation
--(||) :: Bool -> Bool -> Bool
--True || _     =  True
--False || a    = a

fact :: Int -> Int 
fact n 
    | n == 0    = 1
    | n > 0     = n * fact (n - 1)
    | otherwise = error "factorials not defined for negative ints"

mult :: Int -> Int -> Int
mult n m 
    | n == 0        = 0
    | n > 0         = m + mult (n - 1) m 

divide :: Int -> Int -> Int
divide n m
    | n < m         = 0
    | otherwise     = 1 + divide (n - m) m

infixr 3 &&


-- (&&) :: Bool -> Bool -> Bool
-- True && True    = True
-- False && True   = False
-- True && False   = False
-- False && False  = False

-- (&&) :: Bool -> Bool -> Bool
-- True && True = True
-- _ && _       =False

(&&) :: Bool -> Bool -> Bool
True && a    = a
False && _ = False


exOr :: Bool -> Bool -> Bool
exOr True True = False
exOr False False = False
exOr _ _         = True


ifThenElse :: Bool -> Int -> Int -> Int
ifThenElse True a b = a 
ifThenElse False a b = b 

daysInMonth :: Int -> Int
daysInMonth 2 = 28
daysInMonth 4 = 30
daysInMonth 6 = 30
daysInMonth 9 = 30
daysInMonth 11 = 30
daysInMonth _ = 31


-- sumNumbers :: Int -> Int
-- sumNumbers n
--     | n > 0 = n + sumNumbers(n - 1)
--     | n == 0 = 0

sumNumbers :: Int -> Int
sumNumbers 0 = 0
sumNumbers n = n + sumNumbers(n - 1)




-- sumSquares :: Int -> Int
-- sumSquares n 
--     | n > 0 = n^2 + sumSquares(n - 1)
--     | n == 0 = 0

sumSquares :: Int -> Int
sumSquares 0 = 0
sumSquares n = n^2 + sumSquares(n - 1)



-- power :: Int -> Int -> Int
-- power a b
--     | b >  0 = a * power a (b-1)
--     | b == 0 = 1

power :: Int -> Int -> Int
power _ 0 = 1
power a b = a * power a (b-1)

sumFromTo :: Int -> Int -> Int
sumFromTo a b 
    | a > b  = 0
    | a <= b = a + sumFromTo (a+1) b

gcd :: Int -> Int -> Int
gcd a b
    | a == b = a
    | otherwise =  gcd (abs (a - b)) a

 

intSquareRoot :: Int -> Int
intSquareRoot n = findRoot n n

findRoot :: Int -> Int -> Int
findRoot n s 
    | s * s <= n = s
    | otherwise = findRoot n (s-1)
