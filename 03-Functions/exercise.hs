-- 03-Functions Exercise
-- Complete each exercise to practice the concepts from Functions.md
-- 💡 Remember: Once you've completed your solutions, head over to 
--     tester.hs to learn how to test your code!

{-
  1️⃣ EXERCISE 1: Basic Function Definitions & Type Signatures
  Write a function that calculates the area of a circle.
  - Function name: circleArea
  - Input: radius (Float)
  - Output: area (Float)
  - Formula: π * radius²
  - Include the type signature!
-}

-- Write circleArea here:



{-
  2️⃣ EXERCISE 2: Multiple Arguments & Currying
  Write a function that calculates the area of a rectangle.
  Then create two partially applied functions from it.
  
  Part A: Write rectangleArea that takes width and height
  Part B: Create squareArea using partial application (width = height)
  Part C: Create banner using partial application (height = 2)
-}

-- Write rectangleArea here:


-- Create squareArea using partial application:


-- Create banner (height always 2) using partial application:



{-
  3️⃣ EXERCISE 3: Guards
  Write a function that determines shipping cost based on weight:
  - 0-1 kg: $5
  - 1-5 kg: $10  
  - 5-20 kg: $15
  - over 20 kg: $25
  
  Use guards for clean conditional logic!
-}

-- Write shippingCost here:



{-
  4️⃣ EXERCISE 4: Local Definitions with WHERE
  Write a function that calculates the volume and surface area of a cylinder,
  then returns a description string.
  
  - Use WHERE to define helper values for volume and surface area
  - Return format: "Volume: X, Surface Area: Y"
  - Volume formula: π * r² * h
  - Surface Area formula: 2 * π * r² + 2 * π * r * h
-}

-- Write cylinderStats here:



{-
  5️⃣ EXERCISE 5: Local Definitions with LET
  Write a function that solves the quadratic formula: (-b ± √(b²-4ac)) / 2a
  Return both solutions as a tuple: (solution1, solution2)
  
  - Use LET to define helper values for discriminant and denominator
  - solution1 uses + in the numerator
  - solution2 uses - in the numerator
-}

-- Write quadraticSolutions here:



{-
  6️⃣ EXERCISE 6: Function Composition  
  Create a data transformation pipeline using function composition.
  
  Given functions:
  - double: multiply by 2
  - addTen: add 10  
  - square: multiply by itself
  
  Part A: Write the three basic functions
  Part B: Create a composed function that: doubles, then adds ten, then squares
  Part C: Test it with input 3 (should give: ((3*2)+10)² = 16² = 256)
-}

-- Write the basic functions:




-- Create the composed function using (.):



{-
  7️⃣ EXERCISE 7: Function Application Precedence
  Fix these expressions by adding parentheses where needed:
  
  Uncomment each line and add parentheses to make them work correctly:
-}

-- Exercise 7a: We want sqrt of (a² + b²), not (sqrt a) * a + b²
-- pythagorean a b = sqrt a * a + b * b

-- Exercise 7b: We want square of (x + 1), not (square x) + 1  
-- incrementThenSquare x = square x + 1

-- Exercise 7c: We want max of (a + b) and (c + d), not max(a) + b and c + d
-- maxSum a b c d = max a + b c + d


{-
  8️⃣ EXERCISE 8: Putting It All Together
  Write a function that calculates a "fitness score" based on:
  - Age, weight (kg), height (cm), and weekly exercise hours
  
  Rules using guards:
  - If under 18 or over 65: base score is 50
  - If 18-30: base score is 80  
  - If 31-50: base score is 70
  - If 51-65: base score is 60
  
  Then apply modifiers using local definitions:
  - BMI bonus: if BMI < 25, add 10 points
  - Exercise bonus: add (exercise hours * 2) points
  - Height bonus: if height > 180cm, add 5 points
  
  BMI formula: weight / (height in meters)²
-}

-- Write fitnessScore here (use guards + where/let):
