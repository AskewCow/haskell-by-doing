-- 04-Lists-Tuples Exercise
-- Practice with Lists and Tuples

module Main where

{-
📋 === HOW TO TEST YOUR SOLUTIONS ===

1. Complete the exercises below (replace 'undefined' with your code)
2. Open terminal in this folder (04-Lists-Tuples)
3. Run the automated tester: runghc tester.hs
4. See instant results with [PASS] for pass, [FAIL] for fail
5. Fix any issues and test again!

The tester will:
   ✓ Check if your code compiles
   ✓ Test Exercise 1️⃣: Sum of squares calculation
   ✓ Test Exercise 2️⃣: Student grade filtering
   ✓ Test Exercise 3️⃣: Distance and closest point functions
   ✓ Test Exercise 4️⃣: Coordinate pair creation
   ✓ Give you a summary of passed/failed tests

🎉 No manual verification needed - everything is automated!
-}

-- Exercise 1️⃣: Sum of squares
-- Write a function that takes a list of numbers and returns the sum of their squares.
-- sumOfSquares [1,2,3,4] should return 30 (1+4+9+16)

sumOfSquares :: [Int] -> Int
sumOfSquares xs = undefined  -- TODO: Implement this

-- Exercise 2️⃣: Student grades
-- Given a list of student records, find all students with grades above 85
-- excellentStudents [("Alice", 92), ("Bob", 78), ("Charlie", 95)] should return ["Alice", "Charlie"]

type Student = (String, Double)  -- (name, grade)

excellentStudents :: [Student] -> [String]
excellentStudents students = undefined  -- TODO: Implement this

-- Exercise 3️⃣: Tuple operations and distances (🚨 Difficult 🚨)
-- Write a function that calculates the distance between two 2D points
-- Also create a function that finds the point closest to the origin (0,0)
-- distance (0,0) (3,4) should return 5.0
-- closestToOrigin [(1,1), (3,4), (0,1)] should return (0,1)
--
-- HINTS: 
-- • sqrt :: Double -> Double  (square root: sqrt 25.0 = 5.0)
-- • minimum :: [Double] -> Double  (finds smallest: minimum [3,1,4] = 1)

type Point = (Double, Double)

distance :: Point -> Point -> Double
distance p1 p2 = undefined  -- TODO: Implement this using fst and snd

closestToOrigin :: [Point] -> Point
closestToOrigin points = undefined  -- TODO: Implement this (hint: use distance function)

-- Exercise 4️⃣: Zip coordinates
-- Create a list of coordinate pairs from two lists of x and y values
-- makeCoords [1,2,3] [4,5,6] should return [(1,4),(2,5),(3,6)]

makeCoords :: [Double] -> [Double] -> [(Double, Double)]
makeCoords xs ys = undefined  -- TODO: Implement this

{-
📝 === MANUAL TESTING (Optional) ===

You can also test individual functions in GHCi:
1. Type 'ghci exercise.hs' in terminal
2. Test functions: testSumOfSquares, testDistance, etc.
3. Or test directly: sumOfSquares [1,2,3,4]
4. Type ':quit' to exit GHCi
-}

testSumOfSquares = sumOfSquares [1,2,3,4]  -- Should be 30
testExcellentStudents = excellentStudents [("Alice", 92), ("Bob", 78), ("Charlie", 95)]  -- Should be ["Alice", "Charlie"]
testDistance = distance (0,0) (3,4)  -- Should be 5.0
testClosestToOrigin = closestToOrigin [(1,1), (3,4), (0,1)]  -- Should be (0,1)
testMakeCoords = makeCoords [1,2,3] [4,5,6]  -- Should be [(1.0,4.0),(2.0,5.0),(3.0,6.0)]

-- Main function required for compilation
main :: IO ()
main = putStrLn "04-Lists-Tuples Exercise - Use 'runghc tester.hs' to test your solutions!"
