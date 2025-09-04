-- 02-Expressions & Types Solution

module Main where

-- 1️⃣ Exercise 1: GHCi Playground
-- This exercise was about exploring expressions in GHCi.
-- Here are the expressions and their expected results:

{-
GHCi Playground Results:
========================

Expressions to try:
• 2 + 3 * 4        -- Result: 14 (multiplication has higher precedence)
• if True then 1 else 0  -- Result: 1
• "Hi " ++ "there" -- Result: "Hi there"
• not False        -- Result: True
• head "Haskell"   -- Result: 'H'

Type checks:
:t 42              -- Int
:t 'a'             -- Char
:t "hello"         -- [Char] (same as String)
:t True            -- Bool
:t not True        -- Bool
-}

-- 2️⃣ Exercise 2: Even Number Checker
-- Program that asks for a number and prints whether it's even

main :: IO ()
main = do
    putStrLn "Enter a number:"
    input <- getLine
    let number = read input :: Int
    if number `mod` 2 == 0
        then putStrLn "Even"
        else putStrLn "Odd"

-- 3️⃣ Exercise 3: Type Practice
-- Variables with explicit type annotations for each basic type

practiceInt :: Int
practiceInt = 42

practiceInteger :: Integer  
practiceInteger = 123456789012345678901234567890

practiceFloat :: Float
practiceFloat = 3.14

practiceDouble :: Double
practiceDouble = 2.718281828

practiceBool :: Bool
practiceBool = True

practiceChar :: Char
practiceChar = 'H'

practiceString :: String
practiceString = "Hello, Haskell!"

-- 🚀 Bonus Challenges Solutions

-- 1. Enhanced even checker that also tells if number is positive/negative
enhancedChecker :: IO ()
enhancedChecker = do
    putStrLn "Enter a number:"
    input <- getLine
    let number = read input :: Int
    let evenOdd = if number `mod` 2 == 0 then "Even" else "Odd"
    let posNeg = if number > 0 then "Positive" 
                 else if number < 0 then "Negative" 
                 else "Zero"
    putStrLn (evenOdd ++ " and " ++ posNeg)

-- 2. Temperature conversion function
celsiusToFahrenheit :: Double -> Double
celsiusToFahrenheit celsius = celsius * 9 / 5 + 32

fahrenheitToCelsius :: Double -> Double
fahrenheitToCelsius fahrenheit = (fahrenheit - 32) * 5 / 9

-- 3. Examples of type inference (no explicit type annotations)
inferredInt = 100           -- Inferred as Int
inferredString = "Inferred" -- Inferred as String
inferredBool = False        -- Inferred as Bool

-- 4. Examples of read with different type annotations
readExamples :: IO ()
readExamples = do
    putStrLn "Demonstrating read with type annotations:"
    putStrLn ("read \"42\" :: Int = " ++ show (read "42" :: Int))
    putStrLn ("read \"42.5\" :: Double = " ++ show (read "42.5" :: Double))
    putStrLn ("read \"True\" :: Bool = " ++ show (read "True" :: Bool))

-- 💡 Learning Tips:
-- It's totally normal to struggle with Haskell at first! Don't worry if these solutions
-- look confusing initially. Feel free to:
--   • Reference HASKELL-SYNTAX-CHEATSHEET.md in the root folder
--   • Ask AI questions about specific syntax or concepts you don't understand
--   • This course provides the foundation, but AI can help with niche details
-- The combination of structured learning + AI assistance = fastest path to mastery! 🚀
