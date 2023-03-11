import PrimeChecker
import Data.Maybe
import qualified Data.List as L
import Debug.Trace
import Data.List
import Data.Ord


newDigit :: Integer -> Integer -> Integer 
newDigit a b = a*10 + b

newDigitLeft :: Integer -> Integer -> Integer
newDigitLeft a b = a + b* 10

createChain :: (Integer, Integer) -> (Integer, Integer)
--given an int, I will call recursively newdigit that counts the exceptions until i can't add no more. 
-- then, I will store the result and i will keep only the best
createChain (p, 6) =  (stopCondition p, 6)
createChain (p, counter) = trace(show maxsofar) maxsofar
    where newinput = [(newDigit p j ,f (newDigit p j) counter) | j <- [0..9]]
          maxsofar = maximumBy (comparing fst) (fmap (createChain) newinput)
          --max1 = stopCondition p
          f dig ac 
                    | not $ testFermat dig = ac + 1
                    | otherwise = ac
          p' = (maximum $ fmap (fst) (fmap (createChain) newinput))
          


addPrimes :: Integer -> [Integer]
addPrimes a = filter (testFermat) [ newDigit a b | b <- [1,3,7,9]]

stopCondition :: Integer -> Integer
stopCondition a
                | addPrimes a == [] = a
                | otherwise = maximum $ fmap (stopCondition) (addPrimes a)


initialList :: [(Integer, Integer)] 
initialList = [(i, 0) | i <-[2,3,5,7]] ++ [(i, 1) | i <-[0,1,4,6,8,9]]