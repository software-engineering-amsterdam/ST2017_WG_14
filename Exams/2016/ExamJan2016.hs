module ResitExam where 

import Data.List
import System.Random
import Test.QuickCheck

g :: [Int] -> (Int,Int) -> [Int]
g xs (i,j) = take (j-i) (drop i xs)

probs :: Int -> IO [Float]
probs 0 = return []
probs n = do
             p <- getStdRandom random
             ps <- probs (n-1) 
             return (p:ps)

