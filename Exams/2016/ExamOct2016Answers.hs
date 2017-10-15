
module ExamOct2016Answers where

import Data.List
import Test.QuickCheck

type Rel a = [(a,a)]

isAntiSymm :: Eq a => Rel a -> Bool
isAntiSymm rel = and [ x == y | (x,y) <- rel, (y,x) `elem` rel ]

prop_antisymm :: Eq a => Rel a -> Bool
prop_antisymm r = let
   r' = map (\ (x,y) -> (y,x)) r
  in 
   isAntiSymm r == isAntiSymm r'     

prop_antisymm2 :: Eq a => Rel a -> Bool
prop_antisymm2 r = let
   r' = filter (uncurry (/=)) r
  in 
   isAntiSymm r == isAntiSymm r'     

prop_antisymm3 :: Eq a => a -> a -> Rel a -> Bool
prop_antisymm3 x y r =
  x == y || not (isAntiSymm ((x,y):(y,x):r))

data HTree  = Leaf Char Int
            | Fork HTree HTree Int
            deriving (Show)

weight :: HTree -> Int
weight (Leaf _ w)    = w
weight (Fork _ _ w)  = w

prop_huffman :: HTree -> Bool
prop_huffman (Leaf _ _) = True
prop_huffman (Fork t1 t2 w) = prop_huffman t1 && prop_huffman t2 
                     && weight t1 + weight t2 == w

createTree :: [(Char,Int)] -> HTree
createTree []      = error "empty input list"              
createTree [(c,i)] = Leaf c i 
createTree ((c,i):t) = merge (Leaf c i) (createTree t)

merge :: HTree -> HTree -> HTree
merge t1 t2 = Fork t1 t2 (weight t1 + weight t2)

string2tree :: String -> HTree
string2tree = createTree . freqList

freqList :: String -> [(Char,Int)]
freqList s = let 
   units = zip (sort s) (repeat 1)
   f  :: [(Char,Int)] -> [(Char,Int)]
   f [] = []
   f [(x,m)] = [(x,m)]
   f ((x,m):(y,n):zs) | x == y    = f ((x,m+n):zs)
                      | otherwise = (x,m): f ((y,n):zs)
 in 
   f units

ft2string :: [(Char,Int)] -> String
ft2string = concatMap (\ (c,n) -> replicate n c)

prop_table :: String -> Bool
prop_table s = let 
  lst = freqList s 
 in lst == freqList (ft2string lst)

test1 :: IO ()
test1 = quickCheck prop_table

test2 :: IO ()
test2 = quickCheck (\s -> null s || prop_huffman (string2tree s))

probSuccess1 :: Int -> Rational -> Rational        
probSuccess1 k p = let q = 1 - p in
  sum [ p * q^m | m <- [1..k-1] ]

probSuccess2 :: Int -> Rational -> Rational
probSuccess2 k p = 1 - (1 - p)^k

probSuccess3 :: Int -> Rational -> Rational
probSuccess3 k p = let q = 1 - p in
  ((q^k - 1) / (q - 1)) * p

probSuccess1' :: Int -> Rational -> Rational        
probSuccess1' k p = let q = 1 - p in
  sum [ p * q^m | m <- [0..k-1] ]

prop_probsucc12 :: Int -> Rational -> Bool
prop_probsucc12 k p = 
     k < 0 || probSuccess1' k p == probSuccess2 k p

prop_probsucc13 :: Int -> Rational -> Bool
prop_probsucc13 k p =
     k < 0 || probSuccess1' k p == probSuccess3 k p

prop_probsucc23 :: Int -> Rational -> Bool
prop_probsucc23 k p = 
     k < 0 || probSuccess2 k p == probSuccess3 k p

