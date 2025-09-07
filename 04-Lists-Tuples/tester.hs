-- 04-Lists-Tuples Automated Tester
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

-- Test Exercise 1: Sum of squares
testSumOfSquares :: IO TestResult
testSumOfSquares = do
    exerciseContent <- try $ readFile "exercise.hs"
    case exerciseContent of
        Left (e :: SomeException) -> return $ Error $ "Could not read exercise.hs: " ++ show e
        Right content -> 
            if "sumOfSquares xs = undefined" `isInfixOf` content
            then return $ Fail "sumOfSquares is not implemented (still has 'undefined')"
            else do
                -- Create a test file without module declaration conflicts
                let contentWithoutModule = unlines $ filter (not . ("module " `isPrefixOf`)) $ lines content
                let contentWithoutMain = unlines $ filter (not . ("main ::" `isPrefixOf`)) $ filter (not . ("main =" `isPrefixOf`)) $ lines contentWithoutModule
                
                writeFile "temp_sum_test.hs" $ unlines [
                    "-- Test sumOfSquares function",
                    contentWithoutMain,  -- Include exercise content without module/main
                    "",
                    "main :: IO ()",
                    "main = do",
                    "    print (sumOfSquares [1,2,3,4])",  -- Should be 30
                    "    print (sumOfSquares [0,1,2])",    -- Should be 5  
                    "    print (sumOfSquares [])"          -- Should be 0
                    ]
                
                result <- runHaskellFile "temp_sum_test.hs" Nothing
                case result of
                    Left err -> return $ Error err
                    Right output -> do
                        let outputs = lines output
                        if length outputs >= 3 && 
                           "30" `isInfixOf` (outputs !! 0) &&
                           "5" `isInfixOf` (outputs !! 1) &&
                           "0" `isInfixOf` (outputs !! 2)
                        then return Pass
                        else return $ Fail $ "Expected outputs [30,5,0] but got: " ++ show outputs

-- Test Exercise 2: Excellent students
testExcellentStudents :: IO TestResult
testExcellentStudents = do
    exerciseContent <- try $ readFile "exercise.hs"
    case exerciseContent of
        Left (e :: SomeException) -> return $ Error $ "Could not read exercise.hs: " ++ show e
        Right content ->
            if "excellentStudents students = undefined" `isInfixOf` content
            then return $ Fail "excellentStudents is not implemented (still has 'undefined')"
            else do
                let contentWithoutModule = unlines $ filter (not . ("module " `isPrefixOf`)) $ lines content
                let contentWithoutMain = unlines $ filter (not . ("main ::" `isPrefixOf`)) $ filter (not . ("main =" `isPrefixOf`)) $ lines contentWithoutModule
                
                writeFile "temp_students_test.hs" $ unlines [
                    "import Data.List (sort)",
                    "-- Test excellentStudents function",
                    contentWithoutMain,  -- Include exercise content without module/main
                    "",
                    "main :: IO ()",
                    "main = do",
                    "    print (sort (excellentStudents [(\"Alice\", 92), (\"Bob\", 78), (\"Charlie\", 95)]))",
                    "    print (sort (excellentStudents [(\"Dave\", 82), (\"Eve\", 88)]))",
                    "    print (excellentStudents [])"
                    ]
                
                result <- runHaskellFile "temp_students_test.hs" Nothing
                case result of
                    Left err -> return $ Error err
                    Right output -> do
                        let outputs = lines output
                        if length outputs >= 3 &&
                           "[\"Alice\",\"Charlie\"]" `isInfixOf` (outputs !! 0) &&
                           "[\"Eve\"]" `isInfixOf` (outputs !! 1) &&
                           "[]" `isInfixOf` (outputs !! 2)
                        then return Pass
                        else return $ Fail $ "Expected outputs [Alice,Charlie], [Eve], [] but got: " ++ show outputs

-- Test Exercise 3: Distance and closest point
testDistanceAndClosest :: IO TestResult
testDistanceAndClosest = do
    exerciseContent <- try $ readFile "exercise.hs"
    case exerciseContent of
        Left (e :: SomeException) -> return $ Error $ "Could not read exercise.hs: " ++ show e
        Right content -> 
            if "distance p1 p2 = undefined" `isInfixOf` content || 
               "closestToOrigin points = undefined" `isInfixOf` content
            then return $ Fail "distance or closestToOrigin is not implemented (still has 'undefined')"
            else do
                let contentWithoutModule = unlines $ filter (not . ("module " `isPrefixOf`)) $ lines content
                let contentWithoutMain = unlines $ filter (not . ("main ::" `isPrefixOf`)) $ filter (not . ("main =" `isPrefixOf`)) $ lines contentWithoutModule
                
                writeFile "temp_distance_test.hs" $ unlines [
                    "-- Test distance and closestToOrigin functions",
                    contentWithoutMain,  -- Include exercise content without module/main
                    "",
                    "main :: IO ()",
                    "main = do",
                    "    print (distance (0,0) (3,4))",
                    "    print (distance (1,1) (4,5))",
                    "    print (closestToOrigin [(1,1), (3,4), (0,1)])"
                    ]
                
                result <- runHaskellFile "temp_distance_test.hs" Nothing
                case result of
                    Left err -> return $ Error err
                    Right output -> do
                        let outputs = lines output
                        if length outputs >= 3 &&
                           "5.0" `isInfixOf` (outputs !! 0) &&
                           "5.0" `isInfixOf` (outputs !! 1) &&
                           "(0.0,1.0)" `isInfixOf` (outputs !! 2)
                        then return Pass
                        else return $ Fail $ "Expected outputs [5.0, 5.0, (0.0,1.0)] but got: " ++ show outputs

-- Test Exercise 4: Make coordinates
testMakeCoords :: IO TestResult
testMakeCoords = do
    exerciseContent <- try $ readFile "exercise.hs"
    case exerciseContent of
        Left (e :: SomeException) -> return $ Error $ "Could not read exercise.hs: " ++ show e
        Right content ->
            if "makeCoords xs ys = undefined" `isInfixOf` content
            then return $ Fail "makeCoords is not implemented (still has 'undefined')"
            else do
                let contentWithoutModule = unlines $ filter (not . ("module " `isPrefixOf`)) $ lines content
                let contentWithoutMain = unlines $ filter (not . ("main ::" `isPrefixOf`)) $ filter (not . ("main =" `isPrefixOf`)) $ lines contentWithoutModule
                
                writeFile "temp_coords_test.hs" $ unlines [
                    "-- Test makeCoords function",
                    contentWithoutMain,  -- Include exercise content without module/main
                    "",
                    "main :: IO ()",
                    "main = do",
                    "    print (makeCoords [1,2,3] [4,5,6])",
                    "    print (makeCoords [1,2] [4,5,6,7])",
                    "    print (makeCoords [] [1,2,3])"
                    ]
                
                result <- runHaskellFile "temp_coords_test.hs" Nothing
                case result of
                    Left err -> return $ Error err
                    Right output -> do
                        let outputs = lines output
                        if length outputs >= 3 &&
                           "[(1.0,4.0),(2.0,5.0),(3.0,6.0)]" `isInfixOf` (outputs !! 0) &&
                           "[(1.0,4.0),(2.0,5.0)]" `isInfixOf` (outputs !! 1) &&
                           "[]" `isInfixOf` (outputs !! 2)
                        then return Pass
                        else return $ Fail $ "Expected correct coordinate pairs but got: " ++ show outputs

-- Clean up temporary files
cleanupTempFiles :: IO ()
cleanupTempFiles = do
    _ <- try $ removeFile "temp_sum_test.hs" :: IO (Either SomeException ())
    _ <- try $ removeFile "temp_students_test.hs" :: IO (Either SomeException ())
    _ <- try $ removeFile "temp_distance_test.hs" :: IO (Either SomeException ())
    _ <- try $ removeFile "temp_coords_test.hs" :: IO (Either SomeException ())
    _ <- try $ removeFile "exercise.exe" :: IO (Either SomeException ())
    _ <- try $ removeFile "exercise.hi" :: IO (Either SomeException ())
    _ <- try $ removeFile "exercise.o" :: IO (Either SomeException ())
    return ()

-- Main test runner
runAllTests :: IO ()
runAllTests = do
    putStrLn $ blue ++ "=== 04-Lists-Tuples Automated Test Suite ===" ++ reset
    putStrLn $ blue ++ "===========================================" ++ reset
    putStrLn ""
    
    -- Test 1: Compilation
    putStrLn "Testing compilation..."
    compileResult <- testCompilation
    printResult "Compilation Check" compileResult
    putStrLn ""
    
    case compileResult of
        Pass -> do
            -- Test 2: Sum of squares
            putStrLn "[1] Testing Sum of Squares..."
            sumResult <- testSumOfSquares
            printResult "Exercise 1: Sum of Squares" sumResult
            putStrLn ""
            
            -- Test 3: Excellent students
            putStrLn "[2] Testing Excellent Students..."
            studentsResult <- testExcellentStudents
            printResult "Exercise 2: Excellent Students" studentsResult
            putStrLn ""
            
            -- Test 4: Distance and closest point
            putStrLn "[3] Testing Distance and Closest Point..."
            distanceResult <- testDistanceAndClosest
            printResult "Exercise 3: Distance Functions" distanceResult
            putStrLn ""
            
            -- Test 5: Make coordinates
            putStrLn "[4] Testing Make Coordinates..."
            coordsResult <- testMakeCoords
            printResult "Exercise 4: Make Coordinates" coordsResult
            putStrLn ""
            
            -- Clean up temporary files
            cleanupTempFiles
            
            -- Summary
            let results = [compileResult, sumResult, studentsResult, distanceResult, coordsResult]
            let passed = length $ filter (== Pass) results
            let total = length results
            
            putStrLn $ blue ++ "=== Test Summary ===" ++ reset
            putStrLn $ "   " ++ show passed ++ "/" ++ show total ++ " tests passed"
            
            if passed == total
            then putStrLn $ green ++ "SUCCESS: All tests passed! Excellent work on Lists & Tuples!" ++ reset
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
    putStrLn "   [1] Sum of squares: [1,2,3,4] -> 30"
    putStrLn "   [2] Excellent students: filters grades > 85"
    putStrLn "   [3] Distance calculation: (0,0) to (3,4) -> 5.0"
    putStrLn "   [4] Make coordinates: zip two lists into pairs"
    putStrLn ""

main :: IO ()
main = do
    printInstructions
    runAllTests
