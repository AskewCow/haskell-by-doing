-- 05-Pattern-Matching Automated Tester
-- Automated tests to verify your solutions work correctly
-- Uses only built-in Haskell libraries - no external dependencies needed!

module Tester where

import System.IO
import System.Process
import System.Exit
import Control.Exception
import Data.List (isInfixOf, sort, isPrefixOf)
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

-- Test Exercise 1: Factorial
testFactorial :: IO TestResult
testFactorial = do
    exerciseContent <- try $ readFile "exercise.hs"
    case exerciseContent of
        Left (e :: SomeException) -> return $ Error $ "Could not read exercise.hs: " ++ show e
        Right content -> 
            if "factorial = undefined" `isInfixOf` content
            then return $ Fail "factorial is not implemented (still has 'undefined')"
            else do
                let contentLines = lines content
                let contentWithoutModule = filter (not . ("module " `isPrefixOf`)) contentLines
                let contentWithoutMain = filter (\line -> not ("main ::" `isPrefixOf` line || "main =" `isPrefixOf` line)) contentWithoutModule
                
                writeFile "temp_factorial_test.hs" $ unlines [
                    "-- Test factorial function",
                    unlines contentWithoutMain,
                    "",
                    "main :: IO ()",
                    "main = do",
                    "    print (factorial 0)",     -- Should be 1
                    "    print (factorial 1)",     -- Should be 1
                    "    print (factorial 5)",     -- Should be 120
                    "    print (factorial 3)"      -- Should be 6
                    ]
                
                result <- runHaskellFile "temp_factorial_test.hs" Nothing
                case result of
                    Left err -> return $ Error err
                    Right output -> do
                        let outputs = lines output
                        if length outputs >= 4 && 
                           "1" `isInfixOf` (outputs !! 0) &&
                           "1" `isInfixOf` (outputs !! 1) &&
                           "120" `isInfixOf` (outputs !! 2) &&
                           "6" `isInfixOf` (outputs !! 3)
                        then return Pass
                        else return $ Fail $ "Expected outputs [1,1,120,6] but got: " ++ show outputs

-- Test Exercise 2: DescribeList
testDescribeList :: IO TestResult
testDescribeList = do
    exerciseContent <- try $ readFile "exercise.hs"
    case exerciseContent of
        Left (e :: SomeException) -> return $ Error $ "Could not read exercise.hs: " ++ show e
        Right content ->
            if "describeList = undefined" `isInfixOf` content
            then return $ Fail "describeList is not implemented (still has 'undefined')"
            else do
                let contentLines = lines content
                let contentWithoutModule = filter (not . ("module " `isPrefixOf`)) contentLines
                let contentWithoutMain = filter (\line -> not ("main ::" `isPrefixOf` line || "main =" `isPrefixOf` line)) contentWithoutModule
                
                writeFile "temp_describe_test.hs" $ unlines [
                    "-- Test describeList function",
                    unlines contentWithoutMain,
                    "",
                    "main :: IO ()",
                    "main = do",
                    "    print (describeList ([] :: [Int]))",      -- Should be "empty"
                    "    print (describeList [1])",               -- Should be "singleton"
                    "    print (describeList [1,2])",             -- Should be "pair"
                    "    print (describeList [1,2,3,4,5])"        -- Should be "longer list"
                    ]
                
                result <- runHaskellFile "temp_describe_test.hs" Nothing
                case result of
                    Left err -> return $ Error err
                    Right output -> do
                        let outputs = lines output
                        if length outputs >= 4 &&
                           "empty" `isInfixOf` (outputs !! 0) &&
                           "singleton" `isInfixOf` (outputs !! 1) &&
                           "pair" `isInfixOf` (outputs !! 2) &&
                           "longer list" `isInfixOf` (outputs !! 3)
                        then return Pass
                        else return $ Fail $ "Expected outputs [empty, singleton, pair, longer list] but got: " ++ show outputs

-- Test Exercise 3: Safe operations
testSafeOperations :: IO TestResult
testSafeOperations = do
    exerciseContent <- try $ readFile "exercise.hs"
    case exerciseContent of
        Left (e :: SomeException) -> return $ Error $ "Could not read exercise.hs: " ++ show e
        Right content -> 
            if "safeHead = undefined" `isInfixOf` content || "safeTail = undefined" `isInfixOf` content
            then return $ Fail "safeHead or safeTail is not implemented (still has 'undefined')"
            else do
                let contentLines = lines content
                let contentWithoutModule = filter (not . ("module " `isPrefixOf`)) contentLines
                let contentWithoutMain = filter (\line -> not ("main ::" `isPrefixOf` line || "main =" `isPrefixOf` line)) contentWithoutModule
                
                writeFile "temp_safe_test.hs" $ unlines [
                    "-- Test safeHead and safeTail functions",
                    unlines contentWithoutMain,
                    "",
                    "main :: IO ()",
                    "main = do",
                    "    print (safeHead ([] :: [Int]))",        -- Should be Nothing
                    "    print (safeHead [1,2,3])",              -- Should be Just 1
                    "    print (safeTail ([] :: [Int]))",        -- Should be Nothing
                    "    print (safeTail [1,2,3])"               -- Should be Just [2,3]
                    ]
                
                result <- runHaskellFile "temp_safe_test.hs" Nothing
                case result of
                    Left err -> return $ Error err
                    Right output -> do
                        let outputs = lines output
                        if length outputs >= 4 &&
                           "Nothing" `isInfixOf` (outputs !! 0) &&
                           "Just 1" `isInfixOf` (outputs !! 1) &&
                           "Nothing" `isInfixOf` (outputs !! 2) &&
                           "Just [2,3]" `isInfixOf` (outputs !! 3)
                        then return Pass
                        else return $ Fail $ "Expected outputs [Nothing, Just 1, Nothing, Just [2,3]] but got: " ++ show outputs

-- Test Exercise 4: MyLength
testMyLength :: IO TestResult
testMyLength = do
    exerciseContent <- try $ readFile "exercise.hs"
    case exerciseContent of
        Left (e :: SomeException) -> return $ Error $ "Could not read exercise.hs: " ++ show e
        Right content ->
            if "myLength = undefined" `isInfixOf` content
            then return $ Fail "myLength is not implemented (still has 'undefined')"
            else do
                let contentLines = lines content
                let contentWithoutModule = filter (not . ("module " `isPrefixOf`)) contentLines
                let contentWithoutMain = filter (\line -> not ("main ::" `isPrefixOf` line || "main =" `isPrefixOf` line)) contentWithoutModule
                
                writeFile "temp_length_test.hs" $ unlines [
                    "-- Test myLength function",
                    unlines contentWithoutMain,
                    "",
                    "main :: IO ()",
                    "main = do",
                    "    print (myLength ([] :: [Int]))",         -- Should be 0
                    "    print (myLength [1,2,3])",              -- Should be 3
                    "    print (myLength \"hello\")",            -- Should be 5
                    "    print (myLength [1..10])"               -- Should be 10
                    ]
                
                result <- runHaskellFile "temp_length_test.hs" Nothing
                case result of
                    Left err -> return $ Error err
                    Right output -> do
                        let outputs = lines output
                        if length outputs >= 4 &&
                           "0" `isInfixOf` (outputs !! 0) &&
                           "3" `isInfixOf` (outputs !! 1) &&
                           "5" `isInfixOf` (outputs !! 2) &&
                           "10" `isInfixOf` (outputs !! 3)
                        then return Pass
                        else return $ Fail $ "Expected outputs [0,3,5,10] but got: " ++ show outputs

-- Test Exercise 5: MySum
testMySum :: IO TestResult
testMySum = do
    exerciseContent <- try $ readFile "exercise.hs"
    case exerciseContent of
        Left (e :: SomeException) -> return $ Error $ "Could not read exercise.hs: " ++ show e
        Right content ->
            if "mySum = undefined" `isInfixOf` content
            then return $ Fail "mySum is not implemented (still has 'undefined')"
            else do
                let contentLines = lines content
                let contentWithoutModule = filter (not . ("module " `isPrefixOf`)) contentLines
                let contentWithoutMain = filter (\line -> not ("main ::" `isPrefixOf` line || "main =" `isPrefixOf` line)) contentWithoutModule
                
                writeFile "temp_sum_test.hs" $ unlines [
                    "-- Test mySum function",
                    unlines contentWithoutMain,
                    "",
                    "main :: IO ()",
                    "main = do",
                    "    print (mySum [])",                       -- Should be 0
                    "    print (mySum [1,2,3])",                 -- Should be 6
                    "    print (mySum [10,20,30])",              -- Should be 60
                    "    print (mySum [-1,1,-2,2])"              -- Should be 0
                    ]
                
                result <- runHaskellFile "temp_sum_test.hs" Nothing
                case result of
                    Left err -> return $ Error err
                    Right output -> do
                        let outputs = lines output
                        if length outputs >= 4 &&
                           "0" `isInfixOf` (outputs !! 0) &&
                           "6" `isInfixOf` (outputs !! 1) &&
                           "60" `isInfixOf` (outputs !! 2) &&
                           "0" `isInfixOf` (outputs !! 3)
                        then return Pass
                        else return $ Fail $ "Expected outputs [0,6,60,0] but got: " ++ show outputs

-- Test Exercise 6: FirstOfThree
testFirstOfThree :: IO TestResult
testFirstOfThree = do
    exerciseContent <- try $ readFile "exercise.hs"
    case exerciseContent of
        Left (e :: SomeException) -> return $ Error $ "Could not read exercise.hs: " ++ show e
        Right content ->
            if "firstOfThree = undefined" `isInfixOf` content
            then return $ Fail "firstOfThree is not implemented (still has 'undefined')"
            else do
                let contentLines = lines content
                let contentWithoutModule = filter (not . ("module " `isPrefixOf`)) contentLines
                let contentWithoutMain = filter (\line -> not ("main ::" `isPrefixOf` line || "main =" `isPrefixOf` line)) contentWithoutModule
                
                writeFile "temp_triple_test.hs" $ unlines [
                    "-- Test firstOfThree function",
                    unlines contentWithoutMain,
                    "",
                    "main :: IO ()",
                    "main = do",
                    "    print (firstOfThree (\"hello\", 42, True))",    -- Should be "hello"
                    "    print (firstOfThree (100, \"world\", False))",  -- Should be 100
                    "    print (firstOfThree ('x', 1, 2.5))"            -- Should be 'x'
                    ]
                
                result <- runHaskellFile "temp_triple_test.hs" Nothing
                case result of
                    Left err -> return $ Error err
                    Right output -> do
                        let outputs = lines output
                        if length outputs >= 3 &&
                           "\"hello\"" `isInfixOf` (outputs !! 0) &&
                           "100" `isInfixOf` (outputs !! 1) &&
                           "'x'" `isInfixOf` (outputs !! 2)
                        then return Pass
                        else return $ Fail $ "Expected outputs [\"hello\", 100, 'x'] but got: " ++ show outputs

-- Test Exercise 7: ClassifyList
testClassifyList :: IO TestResult
testClassifyList = do
    exerciseContent <- try $ readFile "exercise.hs"
    case exerciseContent of
        Left (e :: SomeException) -> return $ Error $ "Could not read exercise.hs: " ++ show e
        Right content ->
            if "classifyList = undefined" `isInfixOf` content
            then return $ Fail "classifyList is not implemented (still has 'undefined')"
            else do
                let contentLines = lines content
                let contentWithoutModule = filter (not . ("module " `isPrefixOf`)) contentLines
                let contentWithoutMain = filter (\line -> not ("main ::" `isPrefixOf` line || "main =" `isPrefixOf` line)) contentWithoutModule
                
                writeFile "temp_classify_test.hs" $ unlines [
                    "-- Test classifyList function",
                    unlines contentWithoutMain,
                    "",
                    "main :: IO ()",
                    "main = do",
                    "    print (classifyList [])",                    -- Should be "empty list"
                    "    print (classifyList [5])",                  -- Should be "positive singleton"
                    "    print (classifyList [-3])",                 -- Should be "non-positive singleton"
                    "    print (classifyList [1,2,3,4,5,6,7])",      -- Should be "long list"
                    "    print (classifyList [1,2,3])"               -- Should be "short list"
                    ]
                
                result <- runHaskellFile "temp_classify_test.hs" Nothing
                case result of
                    Left err -> return $ Error err
                    Right output -> do
                        let outputs = lines output
                        if length outputs >= 5 &&
                           "empty list" `isInfixOf` (outputs !! 0) &&
                           "positive singleton" `isInfixOf` (outputs !! 1) &&
                           "non-positive singleton" `isInfixOf` (outputs !! 2) &&
                           "long list" `isInfixOf` (outputs !! 3) &&
                           "short list" `isInfixOf` (outputs !! 4)
                        then return Pass
                        else return $ Fail $ "Expected correct classifications but got: " ++ show outputs

-- Test Exercise 8: RemoveAll
testRemoveAll :: IO TestResult
testRemoveAll = do
    exerciseContent <- try $ readFile "exercise.hs"
    case exerciseContent of
        Left (e :: SomeException) -> return $ Error $ "Could not read exercise.hs: " ++ show e
        Right content ->
            if "removeAll = undefined" `isInfixOf` content
            then return $ Fail "removeAll is not implemented (still has 'undefined')"
            else do
                let contentLines = lines content
                let contentWithoutModule = filter (not . ("module " `isPrefixOf`)) contentLines
                let contentWithoutMain = filter (\line -> not ("main ::" `isPrefixOf` line || "main =" `isPrefixOf` line)) contentWithoutModule
                
                writeFile "temp_remove_test.hs" $ unlines [
                    "-- Test removeAll function",
                    unlines contentWithoutMain,
                    "",
                    "main :: IO ()",
                    "main = do",
                    "    print (removeAll 3 [1,3,2,3,4])",         -- Should be [1,2,4]
                    "    print (removeAll 'a' \"banana\")",        -- Should be "bnn"
                    "    print (removeAll 5 [1,2,3,4])",          -- Should be [1,2,3,4]
                    "    print (removeAll 1 [])"                  -- Should be []
                    ]
                
                result <- runHaskellFile "temp_remove_test.hs" Nothing
                case result of
                    Left err -> return $ Error err
                    Right output -> do
                        let outputs = lines output
                        if length outputs >= 4 &&
                           "[1,2,4]" `isInfixOf` (outputs !! 0) &&
                           "\"bnn\"" `isInfixOf` (outputs !! 1) &&
                           "[1,2,3,4]" `isInfixOf` (outputs !! 2) &&
                           "[]" `isInfixOf` (outputs !! 3)
                        then return Pass
                        else return $ Fail $ "Expected correct removal results but got: " ++ show outputs

-- Clean up temporary files
cleanupTempFiles :: IO ()
cleanupTempFiles = do
    _ <- try $ removeFile "temp_factorial_test.hs" :: IO (Either SomeException ())
    _ <- try $ removeFile "temp_describe_test.hs" :: IO (Either SomeException ())
    _ <- try $ removeFile "temp_safe_test.hs" :: IO (Either SomeException ())
    _ <- try $ removeFile "temp_length_test.hs" :: IO (Either SomeException ())
    _ <- try $ removeFile "temp_sum_test.hs" :: IO (Either SomeException ())
    _ <- try $ removeFile "temp_triple_test.hs" :: IO (Either SomeException ())
    _ <- try $ removeFile "temp_classify_test.hs" :: IO (Either SomeException ())
    _ <- try $ removeFile "temp_remove_test.hs" :: IO (Either SomeException ())
    _ <- try $ removeFile "exercise.exe" :: IO (Either SomeException ())
    _ <- try $ removeFile "exercise.hi" :: IO (Either SomeException ())
    _ <- try $ removeFile "exercise.o" :: IO (Either SomeException ())
    return ()

-- Main test runner
runAllTests :: IO ()
runAllTests = do
    putStrLn $ blue ++ "=== 05-Pattern-Matching Automated Test Suite ===" ++ reset
    putStrLn $ blue ++ "===============================================" ++ reset
    putStrLn ""
    
    -- Test 1: Compilation
    putStrLn "Testing compilation..."
    compileResult <- testCompilation
    printResult "Compilation Check" compileResult
    putStrLn ""
    
    case compileResult of
        Pass -> do
            -- Test 2: Factorial
            putStrLn "[1] Testing Factorial..."
            factorialResult <- testFactorial
            printResult "Exercise 1: Factorial" factorialResult
            putStrLn ""
            
            -- Test 3: DescribeList
            putStrLn "[2] Testing DescribeList..."
            describeResult <- testDescribeList
            printResult "Exercise 2: DescribeList" describeResult
            putStrLn ""
            
            -- Test 4: Safe operations
            putStrLn "[3] Testing Safe Operations..."
            safeResult <- testSafeOperations
            printResult "Exercise 3: Safe Head/Tail" safeResult
            putStrLn ""
            
            -- Test 5: MyLength
            putStrLn "[4] Testing MyLength..."
            lengthResult <- testMyLength
            printResult "Exercise 4: MyLength" lengthResult
            putStrLn ""
            
            -- Test 6: MySum
            putStrLn "[5] Testing MySum..."
            sumResult <- testMySum
            printResult "Exercise 5: MySum" sumResult
            putStrLn ""
            
            -- Test 7: FirstOfThree
            putStrLn "[6] Testing FirstOfThree..."
            tripleResult <- testFirstOfThree
            printResult "Exercise 6: FirstOfThree" tripleResult
            putStrLn ""
            
            -- Test 8: ClassifyList
            putStrLn "[7] Testing ClassifyList..."
            classifyResult <- testClassifyList
            printResult "Exercise 7: ClassifyList" classifyResult
            putStrLn ""
            
            -- Test 9: RemoveAll
            putStrLn "[8] Testing RemoveAll..."
            removeResult <- testRemoveAll
            printResult "Exercise 8: RemoveAll" removeResult
            putStrLn ""
            
            -- Clean up temporary files
            cleanupTempFiles
            
            -- Summary
            let results = [compileResult, factorialResult, describeResult, safeResult, lengthResult, sumResult, tripleResult, classifyResult, removeResult]
            let passed = length $ filter (== Pass) results
            let total = length results
            
            putStrLn $ blue ++ "=== Test Summary ===" ++ reset
            putStrLn $ "   " ++ show passed ++ "/" ++ show total ++ " tests passed"
            
            if passed == total
            then putStrLn $ green ++ "SUCCESS: All tests passed! You've mastered Pattern Matching!" ++ reset
            else putStrLn $ yellow ++ "PARTIAL: Keep working on the failing exercises!" ++ reset
                
        _ -> do
            putStrLn $ red ++ "FAILED: Cannot run tests - fix compilation errors first!" ++ reset
            cleanupTempFiles

-- Instructions for the user
printInstructions :: IO ()
printInstructions = do
    putStrLn $ blue ++ "=== How to use this tester ===" ++ reset
    putStrLn "1. Complete your exercises in exercise.hs"
    putStrLn "2. Run this tester: runghc tester.hs"
    putStrLn "3. See which tests pass/fail automatically!"
    putStrLn "4. Fix any issues and test again"
    putStrLn ""
    putStrLn "What this tester checks:"
    putStrLn "   [1] Factorial: 0! = 1, 5! = 120"
    putStrLn "   [2] DescribeList: empty, singleton, pair, longer list"
    putStrLn "   [3] Safe operations: safeHead/safeTail with Maybe types"
    putStrLn "   [4] MyLength: recursive length calculation"
    putStrLn "   [5] MySum: recursive sum with pattern matching"
    putStrLn "   [6] FirstOfThree: extract first element from triple"
    putStrLn "   [7] ClassifyList: combine patterns with guards"
    putStrLn "   [8] RemoveAll: recursive filtering with pattern matching"
    putStrLn ""

main :: IO ()
main = do
    printInstructions
    runAllTests
