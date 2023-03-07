import PrimeChecker
import Data.Maybe
import qualified Data.List as L


newDigit :: Integer -> Integer -> Integer 
newDigit a b = a*10 + b

--this function returns the list of all the chained primes with five exceptions
createChain :: (Integer, Integer)->  [(Integer, Integer)] 
createChain (a ,5) = zip (stopCondition a) (repeat 5)
--createChain a b = Just concat [createChain ((newDigit a j), (f (newDigit a j) b)) | j <- [0..9]]
createChain (a, b) = concat $ fmap (createChain) [(newDigit a j ,f (newDigit a j) b) | j <- [0..9]]
    where  f dig ac 
                   | not $ testFermat dig = ac +1
                   | otherwise = ac

--bigChain :: [(Integer, Integer)]  -> [(Integer, Integer)]
--bigChain [(a, b)] = concat $ fmap (createChain) [(a, b)]

addPrimes :: Integer -> [Integer]
addPrimes a = filter (testFermat) [ newDigit a b | b <- [1,3,7,9]]

stopCondition :: Integer -> [Integer]
stopCondition a
                | addPrimes a == [] = [a]
                | otherwise = concat $ fmap (stopCondition) (addPrimes a)

--start creating a chain: if 

largestPrime :: [(Integer, Integer)] -> Integer
largestPrime [(a, b)] = foldr1 f (map (fst) (concat $ fmap  (createChain) [(a,b)] ))
                        where f num1 num2 = max num1 num2
                            --L.maximumBy h [num1, num2] h num1 num2 = num1 
                     
initialList :: [(Integer, Integer)] 
initialList = [(i, 0) | i <-[0..9]]