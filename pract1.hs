timesTen :: Int -> Int
timesTen x = x * 101

sumThree :: Int -> Int -> Int -> Int
sumThree x y z = x+y+z

areaOfCircle :: Float -> Float
areaOfCircle r = pi * r ^2

areaOfCylinder :: Float -> Float -> Float
areaOfCylinder r h = (pi * r ^2) * h

distance :: Float -> Float -> Float -> Float -> Float
distance x1 y1 x2 y2 = sqrt((y1-y2)^2 + (x1 - x2)^2)

threeDifferent :: Int -> Int -> Int -> Bool
threeDifferent x y z = x /= y && y /= z && x /= z

divisibleBy :: Int -> Int -> Bool
divisibleBy x y = x `mod` y == 0

isEven :: Int -> Bool
isEven x = x `mod` 2 == 0

averageThree :: Int -> Int -> Int -> Float
averageThree x y z = fromIntegral(x + y + z) / 3

absolute :: Int -> Int
absolute x = 
    if x < 0 
        then (-x)
    else 
        x
