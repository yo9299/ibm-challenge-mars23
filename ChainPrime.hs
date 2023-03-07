import PrimeChecker
import Data.Maybe
import qualified Data.List as L


newDigit :: Integer -> Integer -> Integer 
newDigit a b = a*10 + b

createChain :: (Integer, Integer)->  [(Integer, Integer)] 
--createChain (_ ,6) = Nothing
--createChain a b = Just concat [createChain ((newDigit a j), (f (newDigit a j) b)) | j <- [0..9]]
createChain (a, b) = [(newDigit a j ,f (newDigit a j) b) | j <- [0..9]]
    where  f dig ac 
                   | not $ testFermat dig = ac +1
                   | otherwise = ac

bigChain :: [(Integer, Integer)]  -> [[(Integer, Integer)]]
bigChain [(a, b)] =  fmap (createChain) [(a, b)]