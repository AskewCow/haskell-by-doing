-- 🔢 02-Expressions & Types Exercise
-- Complete these exercises after reading the theory

{-
📋 === HOW TO TEST YOUR SOLUTIONS ===

1. Complete the exercises below (replace 'undefined' with your code)
2. Open terminal in this folder (02-Expressions-Types)
3. Run the automated tester: runghc tester.hs
4. See instant results with [PASS] for pass, [FAIL] for fail
5. Fix any issues and test again!

The tester will:
   ✓ Check if your code compiles
   ✗ Test Exercise 1: GHCi playground knowledge (manually compare)
   ✓ Test Exercise 2: Even number checker
   ✓ Test Exercise 3: Type annotations practice
   ✓ Give you a summary of passed/failed tests

🎉 No manual verification needed - everything is automated!
-}

module Main where

-- 1️⃣ Exercise 1: GHCi Playground
-- TODO: Open GHCi (type 'ghci' in terminal) and try these expressions:
--      2 + 3 * 4
--      if True then 1 else 0  (notice: True not true)
--      "Hi " ++ "there"
--      not False
--      head "Haskell"
--
-- Also try these type checks:
--      :t 42
--      :t 'a'
--      :t "hello"
--      :t True
--      :t not True

-- 2️⃣ Exercise 2: Even Number Checker
-- TODO: Write a program that asks for a number and prints whether it's even
-- Use the function `mod :: Int -> Int -> Int` (remainder)

main :: IO ()
main = undefined  -- Replace 'undefined' with your solution

-- 3️⃣ Exercise 3: Type Practice
-- TODO: Create variables with explicit type annotations for each basic type
-- Replace 'undefined' with any valid values of the correct types

practiceInt :: Int
practiceInt = undefined

practiceInteger :: Integer  
practiceInteger = undefined

practiceFloat :: Float
practiceFloat = undefined

practiceDouble :: Double
practiceDouble = undefined

practiceBool :: Bool
practiceBool = undefined

practiceChar :: Char
practiceChar = undefined

practiceString :: String
practiceString = undefined

{-
🚀 === BONUS CHALLENGES ===

1. Modify the even checker to also tell if the number is positive or negative
2. Create a function that converts temperatures between Celsius and Fahrenheit
3. Experiment with type inference by creating variables without type annotations
4. Try using `read` with different type annotations

💡 Remember: If something doesn't make complete sense yet, that's okay!
   We'll cover more advanced topics like functions and polymorphism later.
-}
