module Lab3.Final.Exercises where

import Lab3.Lecture3

-- Define Main --
main = do
    putStrLn $ "===================="
    putStrLn $ "Assignment 3 / Lab 3"
    putStrLn $ "===================="
    putStrLn $ "> Exercise 1"
    exercise1
    putStrLn $ "> Exercise 2"
    -- exercise2
    putStrLn $ "> Exercise 3"
    -- exercise3
    putStrLn $ "> Exercise 4"
    -- exercise4
    putStrLn $ "> Exercise 5"
    -- exercise5

-- | Exercise 1
-- Time spent: 2 hours
-- The implementation mimics as one who would manually check the properties.
-- Writing a new formula above a truth table and evaluation all values of the forms being compared.
-- Then checking, for entailment / equivalence if all values are 'true'
-- We use the forms from Lecture3.hs to check some basic properties of the functions
-- We noticed that for some of our own implementations we simply checked the valuations.
-- This exposed some issues, since two forms can have different input variables.
-- Some implementations were incompatible with different list sizes, furthermore for equal valuation sizes,
-- equal variable names were assumed. Added two explicit test cases which exposed this issue.

exercise1 = do
  putStr "contradictionTest: "
  print contradictionTest
  putStr "tautology: "
  print tautologyTest
  putStr "entails: "
  print entailsTest
  putStr "equivalence: "
  print equivalenceTest
  putStr "Bug fixed with regard to different sizes: "
  print $ True == showSizeIssue
  putStr "Bug fixed with regard to different names: "
  print $ False == showNameIssue

-- | logical contradiction
-- No set of values exists where the form returns true, so if we cannot find a
-- satisfiable set of values the form is a contradiction
contradiction :: Form -> Bool
contradiction f = not $ satisfiable f

-- | logical tautology
-- Instead of finding one set of values which make the form return true like in satisfiable,
-- it should return true for all sets of values
tautology :: Form -> Bool
tautology f = all (`evl` f) (allVals f)

-- | logical entailment
-- When the first form returns true, and the second form returns true, we return true,
-- this is an implication which we should check for every set of values, thus a tautology
entails :: Form -> Form -> Bool
entails f1 f2 = tautology (Impl f1 f2)

-- | logical equivalence
-- Each set of values should return the same value for both forms,
-- which means we can define an equivalence relation,
-- and this should return true for every set of values, so it should be a tautology
equiv :: Form -> Form -> Bool
equiv f1 f2 = tautology (Equiv f1 f2)

-- | Used to expose bug with entailment for different list sizes
showSizeIssue = doEntail "*(1 2)" "+(*(1 2) 3)"

-- | Used to expose bug with entailment for different variable names
showNameIssue = doEntail "*(1 2)" "*(3 4)"

doEntail :: String -> String -> Bool
doEntail f1 f2 = entails (doParse f1) (doParse f2)

doParse :: String -> Form
doParse = head . parse

-- Test contradiction
contradictionTest, tautologyTest, entailsTest, equivalenceTest :: Bool
contradictionTest =  (contradiction form1 == False)
                  && (contradiction form2 == False)
                  && (contradiction form3 == False)

tautologyTest =  (tautology form1 == True)
              && (tautology form2 == False)
              && (tautology form3 == True)

entailsTest =  (entails form1 form1 == True)
            && (entails form1 form2 == False)
            && (entails form1 form3 == True)
            && (entails form2 form1 == True)
            && (entails form2 form2 == True)
            && (entails form2 form3 == True)
            && (entails form3 form1 == True)
            && (entails form3 form2 == False)
            && (entails form3 form3 == True)

equivalenceTest =  (equiv form1 form1 == True)
                && (equiv form1 form2 == False)
                && (equiv form1 form3 == True)
                && (equiv form2 form1 == False)
                && (equiv form2 form2 == True)
                && (equiv form2 form3 == False)
                && (equiv form3 form1 == True)
                && (equiv form3 form2 == False)
                && (equiv form3 form3 == True)


-- | Exercise 2
-- The 'happy day' scenario was tested using a random form generator.
-- All these forms should be eating by the parser and should return a non-empty list
-- When showing the parsed form back to the console, they should be equivalent to the input
-- Generating counter examples proved to be difficult, since the parser exposed different behaviour
-- for different kind of errors.
-- One would expect the parser to return just the form for correct input or nothing at all for incorrect input.
-- Perhaps a nice approach for this would be to have a signature Maybe Form
-- This also allows for parsing an empty string, knowing it is valid tautology, instead of spreading false results
-- Due to parsing partially correct data


-- | This partial result only returns the first term. the implication to the second is missing
partialResultForMissingBrackets = doParse "(1==>2) ==> (1==>3)"

-- | When invalid tokens are received, an exception is thrown
exceptionForIncorrectTokens = doParse "(1<==>3)"

-- | Empty list for tokens which are all partial
emptyListForPartialTokens = parse "((1 2) (3 4))"
