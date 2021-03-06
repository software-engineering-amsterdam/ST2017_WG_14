module ProjectEuler where
import Data.List
import Test.QuickCheck
import Control.Monad
import Data.Bits

prime :: Integer -> Bool
prime n = n > 1 && all (\ x -> rem n x /= 0) xs
  where xs = takeWhile (\ y -> y^2 <= n) primes

primes :: [Integer]
primes = 2 : filter prime [3..]

-- Reusing the prime method from the lecture
-- Time spent: 30 min (on both)
-- 10: simply sum until 2.000.000

solution10 :: Integer
solution10 = sum ( takeWhile (< 2000000) primes )

-- 9: try to create a list and using guards to set the values and the constraints
-- next simply multiply them and get the first (and only) element

solution9 :: Integer
solution9 = head [a * b * c | a <- [1..1000], b <- [a..1000], let c = 1000 - a -b, a^2 + b^2 == c^2]

-- 49: create a list of primes between 1000 and 10000
-- generate a tuple which differs by 3330 and are ale in the primes1000 list
-- check for permutations with built-in elem and permutations function
-- Time spent: 20 min

primes1000 :: [Integer]
primes1000 = dropWhile (< 1000) $ takeWhile (< 10000) primes

digits :: Integer -> [Int]
digits = map (read . return) . show

isPermutation :: (Integer, Integer, Integer) -> Bool
isPermutation (a, b, c) = elem (digits b) (permutations (digits a)) && elem (digits c) (permutations (digits a))

result :: (Integer, Integer, Integer) -> String
result (a, b, c) = (show a) ++ (show b) ++ (show c)

solution49 :: String
solution49 = result $ last $ filter isPermutation [(a , b, c) | a <- primes1000, let b = a + 3330, let c = b + 3330, b `elem` primes1000, c `elem` primes1000]
