-- 01-Fundamentals Solutions

module Main where

-- 1️⃣ Exercise 1: Print "Hello, Haskell!"
main :: IO ()
main = putStrLn "Hello, Haskell!"

-- 2️⃣ Exercise 2: Ask for your name and greet the user
greetUser :: IO ()
greetUser = do
    putStrLn "What is your name?"
    name <- getLine
    putStrLn ("Hello, " ++ name ++ "!")

-- Alternative solution without 'do' notation:
greetUserAlt :: IO ()
greetUserAlt = 
    putStrLn "What is your name?" >>
    getLine >>= \name ->
    putStrLn ("Hello, " ++ name ++ "!")

-- 💡 Learning Tips:
-- It's totally normal to struggle with Haskell at first! Don't worry if these solutions
-- look confusing initially. Feel free to:
--   • Reference HASKELL-SYNTAX-CHEATSHEET.md in the root folder
--   • Ask AI questions about specific syntax or concepts you don't understand
--   • This course provides the foundation, but AI can help with niche details
-- The combination of structured learning + AI assistance = fastest path to mastery! 🚀
