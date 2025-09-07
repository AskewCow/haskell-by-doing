-- 04-Lesson4 Solution

-- Example solution for Lesson 4 exercise.

-- 💡 Learning Tips:
-- It's totally normal to struggle with Haskell at first! Don't worry if these solutions
-- look confusing initially. Feel free to:
--   • Reference HASKELL-SYNTAX-CHEATSHEET.md in the root folder
--   • Ask AI questions about specific syntax or concepts you don't understand
--   • This course provides the foundation, but AI can help with niche details
-- The combination of structured learning + AI assistance = fastest path to mastery! 🚀

-- Exercise 1️⃣: Sum of squares
-- Takes a list of numbers and returns the sum of their squares
sumOfSquares :: [Int] -> Int
sumOfSquares xs = sum [x^2 | x <- xs]
-- Alternative implementation using map and sum:
-- sumOfSquares xs = sum (map (^2) xs)

-- Exercise 2️⃣: Student grades
-- Given a list of student records, find all students with grades above 85
type Student = (String, Double)  -- (name, grade)

excellentStudents :: [Student] -> [String]
excellentStudents students = [fst student | student <- students, snd student > 85]
-- Alternative implementation using filter and map:
-- excellentStudents students = map fst (filter (\student -> snd student > 85) students)

-- Exercise 3️⃣: Tuple operations and distances  
-- Calculate distance between two 2D points and find closest point to origin
type Point = (Double, Double)

distance :: Point -> Point -> Double
distance p1 p2 = sqrt ((fst p2 - fst p1)^2 + (snd p2 - snd p1)^2)
-- This uses fst and snd to extract x and y coordinates from the tuple points

closestToOrigin :: [Point] -> Point
closestToOrigin points = head (filter (\p -> distance (0,0) p == minDist) points)
  where
    distances = map (distance (0,0)) points
    minDist = minimum distances
-- This finds the point with minimum distance to origin (0,0)
-- First calculates all distances, finds the minimum, then filters for that point

-- Exercise 4️⃣: Zip coordinates
-- Create a list of coordinate pairs from two lists of x and y values
makeCoords :: [Double] -> [Double] -> [(Double, Double)]
makeCoords xs ys = zip xs ys
-- The built-in zip function does exactly what we need!


