import Test.QuickCheck
import Data.List
import Prelude

-- Given in lab
prime :: Integer -> Bool
prime n = n > 1 && all (\ x -> rem n x /= 0) xs
   where xs = takeWhile (\ y -> y^2 <= n) primes

primes :: [Integer]
primes = 2 : filter prime [3..] 

primeProduct n = prime $ (+) 1 $ product [1..n]

main = do
-- Smallest counterexample
    print $ head $ filter (not .primeProduct) $ [1..]