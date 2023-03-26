module PrimeChecker where 
import Data.Bits

--calcule b^e (mod m) From trevordixon/modExp.hs
modExp :: Integer -> Integer -> Integer -> Integer 
modExp b 0 m = 1
modExp b e m = t * modExp ((b*b) `mod` m) (shiftR e 1) m `mod` m 
    where t = if testBit e 0 then b `mod` m else 1

testFermat:: Integer -> Bool
testFermat a 
    | a == 0 = True
    | results == [1,1,1,1] = True
    | otherwise = False
    where results = [modExp j (a-1) a | j <- [2,3,5,7]]

isDivisible :: Integer -> Integer -> Bool
isDivisible n a 
    | n `mod` a == 0 = True 
    | otherwise = False

deterministicTest :: Integer -> Bool
deterministicTest a = foldr1 (||) (map f [2..root] )
    where root  =  (round $ sqrt $ fromIntegral a) :: Integer
          f div1 = isDivisible a div1