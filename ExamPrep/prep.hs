import System.Random
import Test.QuickCheck
sentence = "Sentences can go " ++ onAndOn
onAndOn = "on and " ++ onAndOn

sentences = "Sentences can go on" : map (++ " and on") sentences

least1 p = lst p 0
    where lst p n = if p n then n else lst p (n+1)

getRandomInt :: Int -> IO Int
getRandomInt n = getStdRandom (randomR (0, n))

randomFlip :: Int -> IO Int
randomFlip x = do
    b <- getRandomInt 1
    if b == 0 then return x else return (-x)

genIntList :: IO [Int]
genIntList = do
    k <- getRandomInt 20
    n <- getRandomInt 10
    getIntL k n

getIntL :: Int -> Int -> IO [Int]
getIntL _ 0 = return []
getIntL k n = do
    x <- getRandomInt k
    y <- randomFlip x
    xs <- getIntL k (n-1)
    return (y:xs)


testR :: Int -> Int -> ([Int] -> [Int]) -> ([Int] -> [Int] -> Bool) -> IO ()
testR k n f r = if k == n then print (show n ++ " tests passed")
            else do
                xs <- genIntList
                if r xs (f xs) then
                    do 
                    print ("pass on: " ++ show xs)
                    testR (k+1) n f r
                else error ("failed test on: " ++ show xs)

testPost :: ([Int] -> [Int]) -> ([Int] -> Bool) -> IO ()
testPost f p = testR 1 100 f (\_ -> p)

quicksort :: Ord a => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
    quicksort [ a | a <- xs, a <= x ]
    ++ [x]
    ++ quicksort [ a | a <- xs, a > x ]

isTrue :: a -> Bool
isTrue _ = True

prop_ordered :: Ord a => [a] -> Bool
prop_ordered [] = True
prop_ordered (x:xs) = all (>= x) xs && prop_ordered xs

(#) :: (a -> b) -> (b -> c) -> (a -> c)
(#) = flip (.)

($$) :: a -> (a -> b) -> b
($$) = flip ($)


update :: Eq a => (a -> b) -> (a,b) -> a -> b
update f (x, y) = \z -> if x == z then y else f z

updates :: Eq a => (a -> b) -> [(a, b)] -> a -> b
updates = foldl update


while :: ( a-> Bool) -> (a->a) -> a -> a
while = until . (not .)

whiler :: (a -> Bool) -> (a -> a) -> (a -> b) -> a -> b
whiler p f r = while p f # r

fix :: (a -> a) -> a
fix f = f (fix f)