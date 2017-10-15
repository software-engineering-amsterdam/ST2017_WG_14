module ResitExamAnswers where 

import Data.List
import System.Random
import Test.QuickCheck

g :: [Int] -> (Int,Int) -> [Int]
g xs (i,j) = take (j-i) (drop i xs)

maxval :: [Int] -> Int 
maxval xs = let 
   l = length xs 
 in 
   maximum [ sum (g xs (i,j)) | i <- [0..l], j <- [i..l] ]

prop_maxval1 xs = maxval xs == maxval (reverse xs)

prop_maxval2 xs ys = 
  maxval (xs++ys) >= max (maxval xs) (maxval ys)

maxm :: Ord a => [(a,b)] -> b
maxm xs = let 
  ys = sortBy (\ (x,y) (u,v) -> compare u x) xs 
 in 
  snd (head ys)

maxseq :: [Int] -> [Int]
maxseq xs = let 
   l = length xs 
 in 
   maxm [ (sum $ g xs (i,j),g xs (i,j)) | i <- [0..l], j <- [i..l] ]

prop_maxseq xs = maxval xs == sum (maxseq xs)

prop_maxseq2 xs = 
  sum (maxseq xs) == sum (maxseq (reverse xs))

solutions cards = 
  [ xs | xs <- sublists cards, 
         sum xs == product (cards \\ xs) ]

sublists :: [a] -> [[a]]
sublists [] = [[]]
sublists (x:xs) = sublists xs ++ map (x:) (sublists xs)

prop_solutions xs = let 
   xs' = map abs xs
   p   = product xs' 
 in 
   length xs' < 15 ==> elem [p] (solutions (p:xs'))

probs :: Int -> IO [Float]
probs 0 = return []
probs n = do
             p <- getStdRandom random
             ps <- probs (n-1) 
             return (p:ps)

prob2quartile :: Float -> Int
prob2quartile p | p < 0.25  = 1
                | p < 0.50  = 2 
                | p < 0.75  = 3 
                | otherwise = 4

type Count a     = (a, Integer)

addCount :: Eq a => [Count a] -> Count a -> [Count a]
addCount []     y    = [y]
addCount (x:xs) y
    | fst x == fst y = (fst x, snd x + snd y):xs
    | otherwise      = x:addCount xs y

freqCount :: Ord a => [a] -> [Count a]
freqCount xs = sortBy (\ (x,_) (y,_) -> compare x y)
                 (foldl' addCount [] [(x, 1) | x <- xs])

countQuarts :: [Float] -> [Count Int]
countQuarts = freqCount . map prob2quartile

prsInspect = do
               xs <- probs 10000
               return (countQuarts xs)

