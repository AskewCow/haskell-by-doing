-- 📝 05 – Pattern Matching Exercises

module Main where

{-
Welcome to Pattern Matching exercises!

Complete the following functions using pattern matching.
Run `runghc tester.hs` to test your solutions.

Key concepts to practice:
- Matching against literal values
- Using the cons pattern (x:xs) with lists
- Combining pattern matching with guards
- Writing safe, total functions
-}

-- 1️⃣ Exercise 1: Factorial with Pattern Matching
-- Define factorial using pattern matching (not guards!)
-- Remember: factorial(0) = 1, factorial(n) = n * factorial(n-1)
-- Examples: factorial 5 = 120, factorial 0 = 1
factorial :: Int -> Int
factorial = undefined  -- Replace with your implementation

-- 2️⃣ Exercise 2: Describe List Structure
-- Write a function that describes what kind of list it receives
-- Examples:
--   describeList [] = "empty"
--   describeList [1] = "singleton"  
--   describeList [1,2] = "pair"
--   describeList [1,2,3,4,5] = "longer list"
describeList :: [a] -> String
describeList = undefined  -- Replace with your implementation

-- 3️⃣ Exercise 3: Safe List Operations
-- Write safe versions of head and tail that return Maybe values
-- Examples:
--   safeHead [] = Nothing, safeHead [1,2,3] = Just 1
--   safeTail [] = Nothing, safeTail [1,2,3] = Just [2,3]
safeHead :: [a] -> Maybe a
safeHead = undefined  -- Replace with your implementation

safeTail :: [a] -> Maybe [a]
safeTail = undefined  -- Replace with your implementation

-- 4️⃣ Exercise 4: List Length (Recursive)
-- Calculate the length of a list using pattern matching and recursion
-- Examples: myLength [] = 0, myLength [1,2,3] = 3
myLength :: [a] -> Int
myLength = undefined  -- Replace with your implementation

-- 5️⃣ Exercise 5: Sum of Numbers
-- Sum all numbers in a list using pattern matching
-- Examples: mySum [] = 0, mySum [1,2,3] = 6
mySum :: [Int] -> Int
mySum = undefined  -- Replace with your implementation

-- 6️⃣ Exercise 6: Pattern Matching with Tuples
-- Extract the first element from a triple (3-tuple)
-- Example: firstOfThree ("hello", 42, True) = "hello"
firstOfThree :: (a, b, c) -> a
firstOfThree = undefined  -- Replace with your implementation

-- 7️⃣ Exercise 7: Combining Pattern Matching and Guards
-- Classify a list based on its length AND first element (if it exists)
-- Use pattern matching to handle structure, guards for conditions
-- Examples:
--   classifyList [] = "empty list"
--   classifyList [x] where x > 0 = "positive singleton" 
--   classifyList [x] where x <= 0 = "non-positive singleton"
--   classifyList (x:xs) where length xs >= 3 = "long list"
--   classifyList (x:xs) = "short list"
classifyList :: [Int] -> String
classifyList = undefined  -- Replace with your implementation

-- 8️⃣ Exercise 8: Advanced - Remove All Occurrences
-- Remove all instances of a given value from a list
-- Examples: 
--   removeAll 3 [1,3,2,3,4] = [1,2,4]
--   removeAll 'a' "banana" = "bnn" 
removeAll :: Eq a => a -> [a] -> [a]
removeAll = undefined  -- Replace with your implementation

-- 🎯 Bonus Exercise: List Reversal
-- Reverse a list using pattern matching and recursion
-- Example: myReverse [1,2,3] = [3,2,1]
-- Hint: You might need a helper function with an accumulator
myReverse :: [a] -> [a]
myReverse = undefined  -- Replace with your implementation

{-
💡 HINTS:

1. For factorial: Match on 0 first (base case), then n for recursive case

2. For describeList: Use specific patterns like [], [x], [x,y], and (x:xs)

3. For safe functions: Empty list should return Nothing, non-empty returns Just

4. For recursive functions: Always handle the empty list case first!

5. For classifyList: Use pattern matching to destructure, then guards for conditions

6. Remember: pattern order matters! Most specific patterns first.

7. Use _ for values you don't care about: (x, _, _) ignores the middle element

8. The cons pattern (x:xs) is your best friend for list recursion!
-}

-- Main function required for compilation
main :: IO ()
main = putStrLn "05-Pattern-Matching Exercise - Use 'runghc tester.hs' to test your solutions!"
