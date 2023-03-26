import PrimeChecker
import Data.Maybe
import qualified Data.List as L
import Debug.Trace


newDigit :: Integer -> Integer -> Integer 
newDigit a b = a*10 + b

newDigitLeft :: Integer -> Integer -> Integer
newDigitLeft a b = a + b* 10

--this function returns the list of all the chained primes with five exceptions
{-createChain :: (Integer, Integer)->  [(Integer, Integer)] 
createChain (a ,5) | stopCondition a == [] = (a,5)
                   | otherwise zip (stopCondition a) (repeat 5)
--createChain a b = Just concat [createChain ((newDigit a j), (f (newDigit a j) b)) | j <- [0..9]]
createChain (a, b) = concat $ fmap (createChain) [(newDigit a j ,f (newDigit a j) b) | j <- [0..9]]
    where  f dig ac 
                   | not $ testFermat dig = ac +1
                   | otherwise = ac-}

createChain :: (Integer, Integer) -> Integer ->  ([(Integer, Integer)], Integer)
createChain (a ,ex) best 
                        | (ex ==5) && (stopCondition a == []) = trace(show max1) ([(a,5)], max1)
                        | (ex ==5) && (stopCondition a /= []) = trace(show max2) (lastPrime, max2)
                        | otherwise =  trace(show max3) (lastEx, max3)
                            where  f dig ac 
                                        | not $ testFermat dig = ac + 1
                                        | otherwise = ac
                                   lastPrime = zip (stopCondition a) (repeat 5)
                                   max1 = max  (maximum $ fmap fst [(a,5)]) best
                                   max2 = max  (maximum $ fmap fst lastPrime) best
                                   max3 = max (maximum $ bests) best
                                   (lastEx', bests) = unzip $ (zipWith (createChain) [(newDigit a j ,f (newDigit a j) ex) | j <- [0..9]] (repeat best) )
                                   lastEx = concat lastEx'
                                  
                                   


addPrimes :: Integer -> [Integer]
addPrimes a = filter (testFermat) [ newDigit a b | b <- [1,3,7,9]]

stopCondition :: Integer -> [Integer]
stopCondition a
                | addPrimes a == [] = []
                | otherwise = concat $ fmap (stopCondition) (addPrimes a)

--start creating a chain: if 

largestPrime :: [(Integer, Integer)] -> Integer
largestPrime l= maximum bests 
                    where (result, bests )=  unzip $  fmap  (flip createChain 0) l 

                     
initialList :: [(Integer, Integer)] 
initialList = [(i, 0) | i <-[2,3,5,7]] ++ [(i, 1) | i <-[0,1,4,5,6,8,9]]