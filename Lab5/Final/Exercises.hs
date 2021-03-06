module Lab5 where

import Lecture5
import Lecture5NRC
import Lecture5'
import Example
import System.Clock
import Control.Monad
import Data.List
import System.Random (randomRIO)


-- Define Main --
main :: IO ()
main = do
    putStrLn "===================="
    putStrLn "Assignment 5 / Lab 5"
    putStrLn "===================="
    putStrLn "> Exercise 1"
    exercise1
    putStrLn "> Exercise 2"
    exercise2
    putStrLn "> Exercise 3"
    exercise3
    putStrLn "> Exercise 4"
    exercise4
    putStrLn "> Exercise 5"
    exercise5
    putStrLn "> Exercise 6"
    exercise6
    putStrLn "> Exercise 7"
    exercise7

-- =============================================================================
-- Exercise 1 :: Time spent: 4+ hours
-- See Lecture5NRC.hs for code and comments
-- =============================================================================
exercise1 :: IO [()]
exercise1 = do
  solveAndShowNRC example

-- | SOLUTION
-- +-------+-------+-------+
-- | 4 7 8 | 3 9 2 | 6 1 5 |
-- | 6 1 9 | 7 5 8 | 3 2 4 |
-- | 2 3 5 | 4 1 6 | 9 7 8 |
-- +-------+-------+-------+
-- | 7 2 6 | 8 3 5 | 1 4 9 |
-- | 8 9 1 | 6 2 4 | 7 5 3 |
-- | 3 5 4 | 9 7 1 | 2 8 6 |
-- +-------+-------+-------+
-- | 5 6 7 | 2 8 9 | 4 3 1 |
-- | 9 8 3 | 1 4 7 | 5 6 2 |
-- | 1 4 2 | 5 6 3 | 8 9 7 |
-- +-------+-------+-------+

-- =============================================================================
-- Exercise 2 :: Time spent: +- 1.5 hours for refactoring, 1 hour to test
-- See Lecture5NRC'.hs for code and comments

-- The refactored version with the new constraints is more easy to extend, as we
-- only have to add new constraints, instead of adding new functions everywhere
-- to add a new constraint, like in Exercise 1.
--
-- The original version is more efficient at solving problems according to the
-- TimeSpec tests.
--
-- To measure whether the original code, or the refactored code was more
-- efficient we decided to create a function that measures the time required
-- to execute a command. We ran the function for both the original and the
-- refactored code, and arrived at the conclusion that the original code is more
-- efficient at solving code, as shown by the times printed.
--
-- Our functions generate two timespans per algorithm, the first timespan
-- being the time required to generate a problem, the second timespan being the
-- time required to solve a problem.
-- =============================================================================
exercise2 :: IO ()
exercise2 = do
  solveAndShow' example
  res <- testDiff
  print res

testDiff :: IO (Bool, Bool)
testDiff = do
  [r] <- rsolveNs [emptyN]
  showNode r
  refactored <- genProblem' r
  original <- genProblemNRC r
  testRefactored <- testTime $ showNode refactored
  testOriginal <- testTime $ showNode original
  solveRefactored <- testTime $ solveAndShow' (sud2grid $ fst refactored)
  solveOriginal <- testTime $ solveAndShowNRC (sud2grid $ fst original)
  putStrLn "Time taken by original:"
  print testOriginal
  print solveOriginal
  putStrLn "Time taken by refactored:"
  print testRefactored
  print solveRefactored
  return (testRefactored > testOriginal, solveRefactored > solveOriginal)

testTime :: IO a -> IO (TimeSpec)
testTime f = do
  start <- getTime Monotonic
  f
  end <- getTime Monotonic
  return (diffTimeSpec start end)

-- =============================================================================
-- Exercise 3 :: Time spent: +- 3 hours
--
-- How can you test whether the problems generated by the code are minimal?
-- In order to verify this we can simply pass a 'supposed minimal problem' to a tester
-- For each hint, the problem is solved without this hint.
-- The problem was minimal if for every removed hint, the solver finds multiple solutions
-- =============================================================================
exercise3 :: IO ()
exercise3 = do
  putStrLn "Are the generated sudokus minimal?"
  xs <-  (checkerMulti 5)
  let x = and xs
  print x

  {--
  This article helped me understand the problem
  https://www.technologyreview.com/s/426554/mathematicians-solve-minimum-sudoku-problem/
  --}

checker' :: IO Bool
checker' = do
  [r] <- rsolveNs [emptyN]
  s  <- genProblem r
  x <- randomize ( filledPositions (fst s))
  s' <- do
    return (eraseN s (head x))
  return $ not $ uniqueSol s'

checker :: IO Bool
checker = do
  [r] <- rsolveNs [emptyN]
  s  <- genProblem r
  let x  = (filledPositions (fst s))
  let s' = map (eraseNcheck s) x
  return $ and s'

eraseNcheck :: Node -> (Row, Column) ->  Bool
eraseNcheck s pos = not $ uniqueSol (eraseN s pos)

checkerMulti :: Int -> IO [Bool]
checkerMulti n = do
  replicateM n $ checker


-- =============================================================================
-- Exercise 4 :: Time spent: +- 4 hours
--
-- It is possible to generate a Sudoku problems with three empty blocks. In the
-- below code we demonstrate that it is also possible to generate Sudoku problems
-- with four empty blocks. The function to generate Sudoku problems with three
-- empty blocks is also provided. (Change 'do4blocks' to 'do3blocks' in the
-- 'takeM' function).
--
-- There are situations where blocks cannot be removed from the grid, since
-- doing so will cause the problem to become ambiguous. If this occurs, the
-- functions retries until it finds a problem that only has 1 unique solution.
-- =============================================================================
exercise4 :: IO [()]
exercise4 = do
  checkerBlocksMulti 1

checkerBlocks :: IO ()
checkerBlocks = do
  z <- takeM
  showNode z
  let vc = minimalize z (filledPositions (fst z))
  showNode vc

minimizebyBlock ::  Node -> [(Row,Column)] -> Int -> (Node, Int)
minimizebyBlock n [] steps = (n,steps)
minimizebyBlock n ((r,c):rcs) steps | uniqueSol n' = minimizebyBlock n' rcs (steps+1)
                                    | otherwise    = minimizebyBlock n  rcs (steps)
      where n' = eraseN n (r,c)

minimizeUntil :: IO ()
minimizeUntil = do
  z <- takeM
  showNode z

takeM :: IO Node
takeM = do
  [n] <- rsolveNs [emptyN]
  ys <-  do4blocks
  let (n', steps) = (minimizebyBlock n ys 0)
  if steps == (genericLength ys) then
    return n'
  else
    takeM

do3blocks :: IO [(Row,Column)]
do3blocks = do
  let xs = blockConstrnt
  first <- pick ((\\) blockConstrnt [])
  second <- pick ((\\) blockConstrnt [first])
  third <- pick ((\\) blockConstrnt [first, second])
  return $ first ++ second ++ third

do4blocks :: IO [(Row,Column)]
do4blocks = do
  let xs = blockConstrnt
  first <- pick ((\\) blockConstrnt [])
  second <- pick ((\\) blockConstrnt [first])
  third <- pick ((\\) blockConstrnt [first, second])
  fourth <- pick ((\\) blockConstrnt [first, second, third])
  return $ first ++ second ++ third ++ fourth

checkerBlocksMulti :: Int -> IO [()]
checkerBlocksMulti n = do
  replicateM n $ checkerBlocks


-- =============================================================================
-- Exercise 5 :: Time spent: 1+ hour
-- See Lecture5NRC.hs for code and comments
-- =============================================================================
exercise5 :: IO ()
exercise5 = do
  genProblemAndShowNRC

-- =============================================================================
-- Exercise 6 :: Time spent: 1 hour on reading the paper
-- Based on this paper the difficulty of a sudoku is mainly based on two characteristics
-- 1) The techniques required to find the next step => Naked single being most simple
-- 2) The amount of 'next' steps during a moment in the puzzle
-- 3) The amount of solutions => More solutions = more difficult puzzle

-- Can you find a way of classifying the difficulty of a Sudoku problem?
-- The approach to classify these problems uses the 'nextSteps' method, which mimics the 'pencilmarks' technique used by human solvers.
-- If, in the initial solution, there are no nextSteps 1, the problem is definently NO easy solution
-- In order to solve that solution, one must look for places with 2 possibilities and cross one of those.
-- If this succeeds, the Sudoku is 'Simple', otherwise the Sudoku is 'Difficult'
-- Besides this the average number of exposed cells whilst printing is calculated
--
-- Can you modify the Sudoku problem generator so that it can generate problems that are minimal, but easy to solve by hand?
-- We can use the classifications specified in the first problem to generate sudoku's
-- Until no more 'single steps' are found, it removes positions. However, these are not necessarily minimal problems
--
-- Problems that are minimal but hard to solve by hand?
-- The minimal sudoku generator's examples are hard to solve by hand.
-- This is due to the fact that no 'easy techniques' can be applied
-- =============================================================================
exercise6 :: IO ()
exercise6 = do
  putStrLn "Trying to solve a beginner sudoku: "
  solve sudokuBeginner []
  putStrLn "Trying to solve a generated minimal sudoku:"
  solveMinimal
  putStrLn "Simple to solve sudoku:"
  simpleSudoku
  putStrLn "Some hard to solve sudoku:"
  minimalSudoku
  print ()

-- | Simple beginner sudoku
sudokuBeginner :: Sudoku
sudokuBeginner = grid2sud [[9,3,0,1,0,0,0,0,0],
                           [0,7,0,0,4,2,0,5,8],
                           [8,0,0,0,3,7,0,0,0],
                           [0,9,1,0,0,8,6,4,5],
                           [0,6,0,0,0,0,0,8,0],
                           [4,8,7,5,0,0,9,1,0],
                           [0,0,0,8,5,0,0,0,4],
                           [7,5,0,2,6,0,0,9,0],
                           [0,0,0,0,0,4,0,6,2]]

-- | Generate a Sudoku which is simple to solve by hand
simpleSudoku :: IO Sudoku
simpleSudoku = do
  someSolution <- randomSolution
  cleaned <- removeCells someSolution
  putStrLn "Result exercise:"
  showSudoku cleaned
  return cleaned

-- | Minimize sudoku by reverse 'applying' the simple techniques
removeCells :: Sudoku -> IO Sudoku
removeCells sud | ((length $ openPositions sud ) > (length (nextSteps sud))) && ((length (nextSteps sud)) == 1) = do return $ sud
                | otherwise = do
                    removed <- removeCell sud
                    cleaned <- removeCells removed
                    return cleaned

-- | Pick a random step, which is erased
removeCell :: Sudoku -> IO Sudoku
removeCell sud = do
  let cells = filledPositions sud
  someCell <- randomFrom cells
  return $ (eraseS sud someCell)

-- | Picks a random item from the list
randomFrom :: Eq a => [a] -> IO a
randomFrom xs = randomInteger xs >>= (\randIndex -> return (xs !! randIndex))

randomInteger :: Eq a => [a] -> IO Int
randomInteger xs = (randomRIO (0, (length xs)-1))

randomSolution :: IO Sudoku
randomSolution = do
  sud <- genRandomSudoku
  putStrLn "Solution grid:"
  showSudoku (fst sud)
  return $ fst sud

-- | Demonstrates that a minimal sudoku will not solve using the simple techniques
solveMinimal :: IO ()
solveMinimal =  do
    minimal <- minimalSudoku
    solve minimal []

-- | Generator for hard to solve problems
minimalSudoku :: IO Sudoku
minimalSudoku = do
  someSudoku <- genRandomSudoku
  minimized <- genProblem someSudoku
  showSudoku $ fst minimized
  return $ fst minimized

type Step = ((Row,Column), Value)

-- | Solves the sudoku by using the single position technique
solve :: Sudoku -> [Int] -> IO()
solve sud nxts  | isSolved sud = do
                  showSudoku sud
                  putStr "Simple sudoku, Average number of possibilities per step: "
                  print $ div (sum nxts) (length nxts)
                | (nextSteps sud) == [] = do
                    putStr "Difficult Sudoku, unable to solve..."
                | otherwise = do
                  let steps = nextSteps sud
                  solve (update sud (head steps)) (length steps:nxts)


-- | You give it a sudoku, and it hands you the columns with single candidates
nextSteps :: Sudoku -> [Step]
nextSteps sud = [ ((r,c), head values) | r <- [1..9], c <- [1..9], let values = freeAtPos sud (r,c), let size = length values in (size == 1) && ((r,c) `elem` openPositions sud)]

-- | Sudoku is solved when contraints are met
isSolved :: Sudoku -> Bool
isSolved sud = isValid $ sud2grid sud

-- | Simple validator for sudoku grids
squares :: [(Row, Column)]
squares = [(1,1), (1,4), (1,7), (4,1), (4,4), (4,7), (7,1), (7,4), (7,7), (2,2), (2,6), (6,2), (6,6)]

-- | Validate against NRC rules
isValidNrc :: Grid -> Bool
isValidNrc grid = (isValid grid) && (validateSquares grid squares)

-- | Validate against normal rules
isValid :: Grid -> Bool
isValid grid | not $ validGrid grid = False
             | 0 `elem` (concat grid) = False
             | not $ validList grid = False
             | not $ validList (flipGrid grid) = False
             | otherwise = validateSquares grid (take 9 squares)

validateSquares :: Grid -> [(Row,Column)] -> Bool
validateSquares grid squares = all (==True) $ map (validSquare grid) squares

validGrid :: Grid -> Bool
validGrid grid | length grid /= 9 = False
               | (length $ concat grid) /= 81 = False
               | otherwise = True


validList :: Grid -> Bool
validList [] = True
validList (x:xs) = checkValues x && validList xs

-- | Convenience call to flip list of rows to list of columns
flipGrid :: Grid -> Grid
flipGrid grid = composeGrid $ [ value | start <- [1..9], step <- [0..8], let value = (concat grid) !! ((start-1) + (9*step)) ]

composeGrid :: [Value] -> Grid
composeGrid values = [ column | step <- [1..9], let column = drop ((step-1)*9) $ take (step*9) values]

-- | Convenience call to validate a 3x3 grid
validSquare :: Grid -> (Row,Column) -> Bool
validSquare grid (r,c) =  checkValues $ extractSquare grid (r,c)

-- | Takes a 3x3 block from the starting point provided
extractSquare :: Grid -> (Row, Column) -> [Int]
extractSquare grid (r,c) = concat $ map (take 3) $ take 3 $ drop (r-1) $ map (drop (c-1)) grid

-- | checks if all are valid => skip empty cells
checkValues :: [Int] -> Bool
checkValues [] = True
checkValues (x:xs) | (x > 0) && (x `elem` xs) = False
                   | otherwise = checkValues xs


-- =============================================================================
-- Exercise 7 :: Time spent: +- 1 hour
-- =============================================================================
exercise7 :: IO ()
exercise7 = do
  runTestAvgHints 5

runTestAvgHints :: Int -> IO ()
runTestAvgHints n = do
  x <- replicateM n generateAndCountLec
  y <- replicateM n generateAndCountNRC
  putStr "Average number of hints in standard minimal problem: "
  let xAvg =  (fromIntegral (sum x)) / (fromIntegral (genericLength x))
  print xAvg
  putStr "Average number of hints in NRC minimal problem: "
  let yAvg =  (fromIntegral (sum y)) / (fromIntegral (genericLength y))
  print yAvg

generateAndCountLec :: IO Int
generateAndCountLec = do
  [n] <- rsolveNs [emptyN]
  p <- genProblem n
  return $ genericLength $  filledPositions (fst p)

generateAndCountNRC :: IO Int
generateAndCountNRC = do
  [n] <- rsolveNsNRC [emptyN]
  p <- genProblemNRC n
  return $ genericLength$  filledPositions (fst p)

pick :: [a] -> IO a
pick xs = fmap (xs !!) $ randomRIO (0, length xs - 1)
