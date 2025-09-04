-- 03-Functions Solutions

-- 💡 Learning Tips:
-- These exercises are more challenging than previous ones - you're now writing your first
-- complete functions from scratch! It's totally normal to struggle with this step. 
-- Don't worry if these solutions look confusing initially. Feel free to:
--   • Reference HASKELL-SYNTAX-CHEATSHEET.md in the root folder
--   • Ask AI questions about specific syntax or concepts you don't understand
--   • This course provides the foundation, but AI can help with niche details
--   • Take your time - writing functions is a big conceptual leap!
-- The combination of structured learning + AI assistance = fastest path to mastery! 🚀

{-
  1️⃣ SOLUTION 1: Basic Function Definitions & Type Signatures
-}

circleArea :: Float -> Float
circleArea radius = pi * radius * radius

{-
  2️⃣ SOLUTION 2: Multiple Arguments & Currying
-}

-- Part A: Basic function taking two arguments
rectangleArea :: Float -> Float -> Float
rectangleArea width height = width * height

-- Part B: Partial application - width equals height
squareArea :: Float -> Float
squareArea = rectangleArea  -- Since rectangleArea takes width first, 
                            -- this creates a function that uses the same value for both

-- Alternative explicit version:
-- squareArea side = rectangleArea side side

-- Part C: Partial application - height always 2
banner :: Float -> Float
banner = rectangleArea 2  -- Width is fixed at 2, height varies

-- Alternative explicit version:
-- banner height = rectangleArea 2 height

{-
  3️⃣ SOLUTION 3: Guards
-}

shippingCost :: Float -> Float
shippingCost weight
  | weight <= 1   = 5
  | weight <= 5   = 10
  | weight <= 20  = 15
  | otherwise     = 25

{-
  4️⃣ SOLUTION 4: Local Definitions with WHERE
-}

cylinderStats :: Float -> Float -> String
cylinderStats radius height = "Volume: " ++ show volume ++ ", Surface Area: " ++ show surfaceArea
  where
    volume = pi * radius^2 * height
    surfaceArea = 2 * pi * radius^2 + 2 * pi * radius * height

{-
  5️⃣ SOLUTION 5: Local Definitions with LET
-}

quadraticSolutions :: Float -> Float -> Float -> (Float, Float)
quadraticSolutions a b c =
    let discriminant = b^2 - 4*a*c
        denominator = 2*a
        sqrtDiscriminant = sqrt discriminant
    in ((-b + sqrtDiscriminant) / denominator, (-b - sqrtDiscriminant) / denominator)

{-
  6️⃣ SOLUTION 6: Function Composition
-}

-- Part A: Basic functions
double :: Int -> Int
double x = x * 2

addTen :: Int -> Int
addTen x = x + 10

square :: Int -> Int
square x = x * x

-- Part B: Composed function (reads right to left)
transform :: Int -> Int
transform = square . addTen . double
-- This means: first double, then addTen, then square

{-
  7️⃣ SOLUTION 7: Function Application Precedence
-}

-- Exercise 7a: We want sqrt of (a² + b²), not (sqrt a) * a + b²
pythagorean :: Float -> Float -> Float
pythagorean a b = sqrt (a * a + b * b)

-- Exercise 7b: We want square of (x + 1), not (square x) + 1  
incrementThenSquare :: Int -> Int
incrementThenSquare x = square (x + 1)

-- Exercise 7c: We want max of (a + b) and (c + d), not max(a) + b and c + d
maxSum :: Int -> Int -> Int -> Int -> Int
maxSum a b c d = max (a + b) (c + d)

{-
  8️⃣ SOLUTION 8: Putting It All Together
-}

fitnessScore :: Int -> Float -> Float -> Float -> Float
fitnessScore age weight height exerciseHours = baseScore + bmiBonus + exerciseBonus + heightBonus
  where
    -- Base score using guards
    baseScore
      | age < 18 || age > 65  = 50
      | age <= 30             = 80
      | age <= 50             = 70
      | otherwise             = 60
    
    -- Helper calculations
    heightInMeters = height / 100
    bmi = weight / (heightInMeters^2)
    
    -- Bonuses
    bmiBonus = if bmi < 25 then 10 else 0
    exerciseBonus = exerciseHours * 2
    heightBonus = if height > 180 then 5 else 0

{-
  📝 ALTERNATIVE SOLUTIONS & VARIATIONS:

  Here are some alternative ways to write some of these functions:

  -- Alternative circleArea using where:
  circleAreaAlt :: Float -> Float
  circleAreaAlt radius = pi * radiusSquared
    where radiusSquared = radius * radius

  -- Alternative shippingCost using nested if-then-else (not recommended):
  shippingCostAlt :: Float -> Float
  shippingCostAlt weight = 
    if weight <= 1 then 5
    else if weight <= 5 then 10
    else if weight <= 20 then 15
    else 25

  -- Alternative quadraticSolutions using where:
  quadraticSolutionsAlt :: Float -> Float -> Float -> (Float, Float)
  quadraticSolutionsAlt a b c = (solution1, solution2)
    where
      discriminant = b^2 - 4*a*c
      denominator = 2*a
      sqrtDiscriminant = sqrt discriminant
      solution1 = (-b + sqrtDiscriminant) / denominator
      solution2 = (-b - sqrtDiscriminant) / denominator

  -- Point-free style for some functions (advanced):
  doublePointFree :: Int -> Int
  doublePointFree = (*2)
  
  addTenPointFree :: Int -> Int
  addTenPointFree = (+10)
-}
