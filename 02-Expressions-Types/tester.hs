-- 02-Expressions & Types Automated Tester
-- Automated tests to verify your solutions work correctly
-- Uses only built-in Haskell libraries - no external dependencies needed!

module Tester where

import System.IO
import System.Process
import System.Exit
import Control.Exception
import Data.List (isInfixOf)

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

-- Test Exercise 2: Even Number Checker
testEvenChecker :: IO TestResult
testEvenChecker = do
    -- Test with input "4" (should output "Even")
    result1 <- runHaskellFile "exercise.hs" (Just "4\n")
    case result1 of
        Left err -> return $ Error err
        Right output1 -> 
            if "Even" `isInfixOf` output1 then do
                -- Test with input "3" (should output "Odd")
                result2 <- runHaskellFile "exercise.hs" (Just "3\n")
                case result2 of
                    Left err -> return $ Error err
                    Right output2 ->
                        if "Odd" `isInfixOf` output2
                        then return Pass
                        else return $ Fail $ "Input '3' should output 'Odd', but got: " ++ show (take 50 output2)
            else return $ Fail $ "Input '4' should output 'Even', but got: " ++ show (take 50 output1)

-- Test Exercise 3: Check if type practice variables are implemented
testTypePractice :: IO TestResult
testTypePractice = do
    exerciseContent <- try $ readFile "exercise.hs"
    case exerciseContent of
        Left (e :: SomeException) -> return $ Error $ "Could not read exercise.hs: " ++ show e
        Right content -> do
            let hasInt = "practiceInt :: Int" `isInfixOf` content && not ("practiceInt = undefined" `isInfixOf` content)
            let hasInteger = "practiceInteger :: Integer" `isInfixOf` content && not ("practiceInteger = undefined" `isInfixOf` content)
            let hasFloat = "practiceFloat :: Float" `isInfixOf` content && not ("practiceFloat = undefined" `isInfixOf` content)
            let hasDouble = "practiceDouble :: Double" `isInfixOf` content && not ("practiceDouble = undefined" `isInfixOf` content)
            let hasBool = "practiceBool :: Bool" `isInfixOf` content && not ("practiceBool = undefined" `isInfixOf` content)
            let hasChar = "practiceChar :: Char" `isInfixOf` content && not ("practiceChar = undefined" `isInfixOf` content)
            let hasString = "practiceString :: String" `isInfixOf` content && not ("practiceString = undefined" `isInfixOf` content)
            
            let implemented = [hasInt, hasInteger, hasFloat, hasDouble, hasBool, hasChar, hasString]
            let implementedCount = length $ filter id implemented
            
            if implementedCount == 7
            then return Pass
            else return $ Fail $ "Only " ++ show implementedCount ++ "/7 type practice variables are implemented (not undefined)"

-- Main test runner
runAllTests :: IO ()
runAllTests = do
    putStrLn $ blue ++ "=== 02-Expressions & Types Automated Test Suite ===" ++ reset
    putStrLn $ blue ++ "=================================================" ++ reset
    putStrLn ""
    
    -- Test 1: Compilation
    putStrLn "Testing compilation..."
    compileResult <- testCompilation
    printResult "Compilation Check" compileResult
    putStrLn ""
    
    case compileResult of
        Pass -> do
            -- Test 2: Even Number Checker
            putStrLn "Testing Even Number Checker..."
            evenResult <- testEvenChecker
            printResult "Even Number Checker" evenResult
            putStrLn ""
            
            -- Test 3: Type Practice
            putStrLn "Testing Type Practice variables..."
            typeResult <- testTypePractice
            printResult "Type Practice Variables" typeResult
            putStrLn ""
            
            -- Summary
            let results = [compileResult, evenResult, typeResult]
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
    putStrLn "Note: Exercise 1 (GHCi Playground) is manual - try the expressions in GHCi yourself!"
    putStrLn ""

main :: IO ()
main = do
    printInstructions
    runAllTests
