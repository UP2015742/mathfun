absolute :: Int -> Int
absolute x 
    | x < 0 = (-x)
    | otherwise  = x

sign :: Int -> Int 
sign x
    | x < 0 = (-1)
    | x > 0 = 1
    | otherwise = 0

howManyEqual :: Int -> Int -> Int -> Int 
howManyEqual x y z
    | x == y && y == z = 3
    | x /= y && y /= z && x /= z = 0
    |otherwise = 2

sumDiagonalLengths :: Float -> Float -> Float -> Float
sumDiagonalLengths x y z = xd + yd + zd
    where 
    xd = sqrt((x ^ 2) * 2)
    yd = sqrt((y ^ 2) * 2)
    zd = sqrt((z ^ 2) * 2)


taxiFare :: Int -> Float
taxiFare k 
            | k > 10    = fromIntegral (k - 10) * 0.3 + 5 + 2.2
            | otherwise = fromIntegral (k) * 0.5 + 2.2




howManyAboveAverage :: Int -> Int -> Int -> Int
howManyAboveAverage x y z 
  | fromIntegral(maximum[x, y, z]) == avg = 0
  | fromIntegral(x) > avg && fromIntegral(y) > avg = 2
  |fromIntegral(x) > avg && fromIntegral(z) > avg = 2
  |fromIntegral(y) > avg && fromIntegral(z) > avg = 2
  |otherwise = 1
  where
  avg = fromIntegral(x + y + z) / 3



  validDate :: Int -> Int -> Bool
  validDate d m 
    | d == 29 && m == 13 = True
    | otherwise = False