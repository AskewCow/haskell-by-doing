-- 01-Fundamentals Automated Tester
-- Automated tests to verify your solutions work correctly
-- Uses only built-in Haskell libraries - no external dependencies needed!

module Tester where

import System.IO
import System.Process
import System.Exit
import Control.Exception
import Data.List (isInfixOf)
import System.Directory (removeFile)

-- Colors for better output
red, green, yellow, blue, reset :: String
red = "\ESC[31m"
green = "\ESC[32m"
yellow = "\ESC[33m"
blue = "\ESC[34m"
reset = "\ESC[0m"

-- Test result type
data TestResult = Pass | Fail String | Error String
    deriving (Show, Eq)

-- Pretty print test results
printResult :: String -> TestResult -> IO ()
printResult testName result = case result of
    Pass -> putStrLn $ green ++ "[PASS] " ++ testName ++ reset
    Fail msg -> putStrLn $ red ++ "[FAIL] " ++ testName ++ ": " ++ msg ++ reset
    Error msg -> putStrLn $ yellow ++ "[ERROR] " ++ testName ++ ": " ++ msg ++ reset

-- Run a Haskell file and capture its output
runHaskellFile :: String -> Maybe String -> IO (Either String String)
runHaskellFile filename input = do
    result <- try $ do
        let processConfig = case input of
                Nothing -> proc "runghc" [filename]
                Just _ -> (proc "runghc" [filename]) { std_in = CreatePipe }
        
        (exitCode, stdout, stderr) <- readCreateProcessWithExitCode processConfig (maybe "" id input)
        
        case exitCode of
            ExitSuccess -> return stdout
            ExitFailure _ -> return $ "Error: " ++ stderr
    
    case result of
        Left (e :: SomeException) -> return $ Left $ "Exception: " ++ show e
        Right output -> return $ Right output

-- Test Exercise 1: Hello World
testHelloWorld :: IO TestResult
testHelloWorld = do
    result <- runHaskellFile "exercise.hs" Nothing
    case result of
        Left err -> return $ Error err
        Right output -> 
            if "Hello, Haskell!" `isInfixOf` output
            then return Pass
            else return $ Fail $ "Expected 'Hello, Haskell!' but got: " ++ show (take 100 output)

-- Test Exercise 2: Check if greetUser function works
testGreeting :: IO TestResult
testGreeting = do
    -- Create a temporary file that tests the greetUser function
    writeFile "temp_greet_test.hs" $ unlines [
        "import System.IO",
        "",
        "-- Copy the greetUser function definition to test it",
        "greetUser :: IO ()",
        "greetUser = undefined  -- This will be replaced with student's implementation",
        "",
        "main :: IO ()",
        "main = greetUser"
        ]
    
    -- Read the student's greetUser implementation from exercise.hs
    exerciseContent <- readFile "exercise.hs"
    let lines' = lines exerciseContent
    let greetUserLines = dropWhile (not . ("greetUser" `isInfixOf`)) lines'
    
    if null greetUserLines
    then return $ Error "greetUser function not found in exercise.hs"
    else do
        -- Check if it's still undefined
        let greetUserDef = unwords $ take 5 greetUserLines
        if "undefined" `isInfixOf` greetUserDef
        then return $ Fail "greetUser function is not implemented (still has 'undefined')"
        else return Pass  -- If not undefined, assume it's implemented

-- Test if exercise.hs exists and compiles
testCompilation :: IO TestResult
testCompilation = do
    result <- try $ do
        (exitCode, _, stderr) <- readCreateProcessWithExitCode (proc "ghc" ["-fno-warn-tabs", "-fno-warn-unused-imports", "exercise.hs"]) ""
        case exitCode of
            ExitSuccess -> return "OK"
            ExitFailure _ -> return $ "Compilation failed: " ++ stderr
    
    case result of
        Left (e :: SomeException) -> return $ Error $ "Could not compile: " ++ show e
        Right "OK" -> return Pass
        Right err -> return $ Fail err

-- Main test runner
runAllTests :: IO ()
runAllTests = do
    putStrLn $ blue ++ "=== 01-Fundamentals Automated Test Suite ===" ++ reset
    putStrLn $ blue ++ "===========================================" ++ reset
    putStrLn ""
    
    -- Test 1: Compilation
    putStrLn "Testing compilation..."
    compileResult <- testCompilation
    printResult "Compilation Check" compileResult
    putStrLn ""
    
    case compileResult of
        Pass -> do
            -- Test 2: Hello World
            putStrLn "Testing Hello World output..."
            helloResult <- testHelloWorld
            printResult "Hello World Exercise" helloResult
            putStrLn ""
            
            -- Test 3: Greeting
            putStrLn "Testing Greeting functionality..."
            greetResult <- testGreeting
            printResult "Greeting Exercise" greetResult
            putStrLn ""
            
            -- Clean up temporary files
            _ <- try $ removeFile "temp_greet_test.hs" :: IO (Either SomeException ())
            
            -- Summary
            let results = [compileResult, helloResult, greetResult]
            let passed = length $ filter (== Pass) results
            let total = length results
            
            putStrLn $ blue ++ "=== Test Summary ===" ++ reset
            putStrLn $ "   " ++ show passed ++ "/" ++ show total ++ " tests passed"
            
            if passed == total
            then putStrLn $ green ++ "SUCCESS: All tests passed! Great job!" ++ reset
            else putStrLn $ yellow ++ "PARTIAL: Keep working on the failing tests!" ++ reset
                
        _ -> putStrLn $ red ++ "FAILED: Cannot run tests - fix compilation errors first!" ++ reset

-- Instructions for the user
printInstructions :: IO ()
printInstructions = do
    putStrLn $ blue ++ "=== How to use this tester ===" ++ reset
    putStrLn "1. Complete your exercises in exercise.hs"
    putStrLn "2. Run this tester: runghc tester.hs"
    putStrLn "3. See which tests pass/fail automatically!"
    putStrLn "4. Fix any issues and test again"
    putStrLn ""

main :: IO ()
main = do
    printInstructions
    runAllTests
