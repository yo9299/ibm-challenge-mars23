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

createChain :: (Integer, Int) -> Integer -> Integer
createChain (prefix,5) best = continueWithoutException prefix best
createChain (prefix, counter) best = foldl' f best $ primepref ++ notprimepref
    where newprefixes = fmap (newDigit prefix) [0..9]
          primepref = zip primepref_ $ repeat counter
          notprimepref = zip notprimepref_ $ repeat (counter +1)
          (primepref_, notprimepref_) = partition testFermat newprefixes
          f currbest n = createChain n currbest




{---given an int, I will call recursively newdigit that counts the exceptions until i can't add no more. 
-- then, I will store the result and i will keep only the best
createChain (p, 5) =  (continueWithoutException p, 5)
createChain (p, counter) = trace(show track) maxsofar
    where newinput = [(newDigit p j ,f (newDigit p j) counter) | j <- [0..9]]
          track = max p (fst maxsofar)
          maxsofar = maximumBy (comparing fst) (fmap (createChain) newinput)
          --max1 = continueWithoutException p
          f dig ac 
                    | not $ testFermat dig = ac + 1
                    | otherwise = ac
          --p' = (maximum $ fmap (fst) (fmap (createChain) newinput))-}
          


{-addPrimes :: Integer -> [Integer]
addPrimes a = filter (testFermat) [ newDigit a b | b <- [1,3,7,9]]-}

continueWithoutException :: Integer -> Integer -> Integer
continueWithoutException prefix best
                | addPrimes == [] = let nbest = max prefix best in trace (show (nbest,logBase 10 $ fromIntegral nbest)) nbest
                | otherwise = foldl' f best addPrimes

                    --maximum $ fmap (continueWithoutException) (addPrimes prefix)
    where addPrimes  = filter (testFermat) [ newDigit prefix b | b <- [1,3,7,9]]
          f currbest n = continueWithoutException n currbest


initialList :: [(Integer, Integer)] 
initialList = [(i, 0) | i <-[2,3,5,7]] 

initialList2 :: [(Integer, Integer)]
initialList2 = [(i, 1) | i <-[1,4,6,8,9]]

-- 78797339359331171479990333 26
-- 238339393727993993999713991 27
-- 3733799911799539139382193991
-- 3733799911799539139382193991, 28
-- 5939337193981799393983973    25
-- 972137331717733217999
-- 1979573399093333341399193    25
-- 43972337923399309813793      23
-- 61990931333977939731731      23
-- 82939939933339137793939
-- 972137331717733217999 