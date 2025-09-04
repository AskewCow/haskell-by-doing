-- 03-Functions Testing Tutorial
-- Learn to test your own code using GHCi!

{-
  🧪 WHY LEARN TO TEST YOUR OWN CODE?
  
  Testing is a crucial skill in programming. By learning to test your functions manually
  in GHCi, you'll:
  • Understand how your functions actually behave
  • Catch bugs early and fix them quickly  
  • Build confidence in your solutions
  • Develop debugging skills
  • Learn to think about edge cases and different inputs
  
  This skill will serve you well throughout your programming journey!
-}

{-
  📋 HOW TO USE THIS TESTING TUTORIAL:
  
  1. Complete your exercises in exercise.hs
  2. Load your exercise file in GHCi: :load exercise.hs
  3. Follow the testing examples below to verify your solutions
  4. If something doesn't work, debug and fix it!
  5. Compare with solution.hs once you're confident your code works
-}

{-
  🚀 GETTING STARTED WITH GHCi TESTING:
  
  Step 1: Open a terminal/command prompt
  Step 2: Navigate to the 03-Functions folder
  Step 3: Start GHCi by typing: ghci
  Step 4: Load your exercise file: :load exercise.hs
  Step 5: Start testing!
  
  💡 Pro tip: If you make changes to your file, reload it with :r
-}

{-
  1️⃣ TESTING EXERCISE 1: circleArea
  
  What to test:
  • Basic functionality with known values
  • Edge cases (like radius = 0)
  
  Example tests to try in GHCi:
  
  ghci> circleArea 0        -- Should be 0.0
  ghci> circleArea 1        -- Should be π ≈ 3.14159
  ghci> circleArea 2        -- Should be 4π ≈ 12.566
  ghci> circleArea 5        -- Should be 25π ≈ 78.54
  
  💡 Remember: π (pi) is built into Haskell, so your results should be accurate!
  
  🤔 Think about it:
  • What happens with negative radius? (Try it!)
  • Does the result make sense mathematically?
-}

{-
  2️⃣ TESTING EXERCISE 2: rectangleArea, squareArea, banner
  
  Testing rectangleArea:
  ghci> rectangleArea 4 6      -- Should be 24.0
  ghci> rectangleArea 0 10     -- Should be 0.0 (edge case)
  ghci> rectangleArea 5 5      -- Should be 25.0 (square)
  
  Testing squareArea (partial application):
  ghci> squareArea 4           -- Should be 16.0
  ghci> squareArea 0           -- Should be 0.0
  ghci> squareArea 10          -- Should be 100.0
  
  Testing banner (partial application):
  ghci> banner 5               -- Should be 10.0 (2 * 5)
  ghci> banner 0               -- Should be 0.0
  ghci> banner 3               -- Should be 6.0
  
  🎯 Key learning: Notice how partial application creates new functions!
  • squareArea is really rectangleArea with width = height
  • banner is really rectangleArea with width fixed at 2
-}

{-
  3️⃣ TESTING EXERCISE 3: shippingCost
  
  Test each weight range:
  ghci> shippingCost 0.5       -- Should be 5 (0-1 kg range)
  ghci> shippingCost 1.0       -- Should be 5 (exactly 1 kg)
  ghci> shippingCost 3.0       -- Should be 10 (1-5 kg range)
  ghci> shippingCost 5.0       -- Should be 10 (exactly 5 kg)
  ghci> shippingCost 10.0      -- Should be 15 (5-20 kg range)
  ghci> shippingCost 20.0      -- Should be 15 (exactly 20 kg)
  ghci> shippingCost 25.0      -- Should be 25 (over 20 kg)
  
  🎯 Key learning: Test the boundaries! 
  • What happens exactly at 1, 5, and 20 kg?
  • This is where bugs often hide in guard conditions
-}

{-
  4️⃣ TESTING EXERCISE 4: cylinderStats
  
  ghci> cylinderStats 1 1      -- Small cylinder
  ghci> cylinderStats 3 10     -- Tall cylinder  
  ghci> cylinderStats 5 2      -- Wide, short cylinder
  ghci> cylinderStats 0 10     -- Edge case: no radius
  
  Expected format: "Volume: X, Surface Area: Y"
  
  🧮 Manual calculation check:
  For radius=1, height=1:
  • Volume = π × 1² × 1 = π ≈ 3.14159
  • Surface Area = 2π × 1² + 2π × 1 × 1 = 4π ≈ 12.566
  
  🎯 Key learning: 
  • Check that your string formatting works correctly
  • Verify the mathematical formulas are right
-}

{-
  5️⃣ TESTING EXERCISE 5: quadraticSolutions
  
  Test with known solutions:
  ghci> quadraticSolutions 1 (-5) 6    -- Should be (3.0, 2.0)
  ghci> quadraticSolutions 1 (-3) 2    -- Should be (2.0, 1.0) 
  ghci> quadraticSolutions 1 0 (-4)    -- Should be (2.0, -2.0)
  
  🧮 Manual verification for x² - 5x + 6 = 0:
  • Solutions should be x = 3 and x = 2
  • Check: 3² - 5(3) + 6 = 9 - 15 + 6 = 0 ✓
  • Check: 2² - 5(2) + 6 = 4 - 10 + 6 = 0 ✓
  
  🎯 Key learning:
  • Always verify mathematical functions with known answers
  • The order of solutions in the tuple matters
-}

{-
  6️⃣ TESTING EXERCISE 6: double, addTen, square, transform
  
  Test individual functions first:
  ghci> double 5               -- Should be 10
  ghci> addTen 5               -- Should be 15  
  ghci> square 5               -- Should be 25
  
  Test composition:
  ghci> transform 3            -- Should be 256
  
  🧮 Step-by-step verification of transform 3:
  • double 3 = 6
  • addTen 6 = 16  
  • square 16 = 256
  
  More tests:
  ghci> transform 0            -- Should be 100 (square(addTen(double(0))))
  ghci> transform 1            -- Should be 144 (square(addTen(double(1))))
  
  🎯 Key learning: Function composition reads right-to-left!
-}

{-
  7️⃣ TESTING EXERCISE 7: pythagorean, incrementThenSquare, maxSum
  
  ghci> pythagorean 3 4        -- Should be 5.0 (classic 3-4-5 triangle)
  ghci> pythagorean 5 12       -- Should be 13.0 (5-12-13 triangle)
  ghci> pythagorean 0 0        -- Should be 0.0
  
  ghci> incrementThenSquare 3  -- Should be 16 (square of 4)
  ghci> incrementThenSquare 0  -- Should be 1 (square of 1)
  
  ghci> maxSum 1 2 3 4         -- Should be 7 (max of 3 and 7)
  ghci> maxSum 10 0 5 5        -- Should be 10 (max of 10 and 10)
  
  🎯 Key learning: Parentheses matter for function precedence!
-}

{-
  8️⃣ TESTING EXERCISE 8: fitnessScore
  
  Test different age ranges:
  ghci> fitnessScore 16 60 170 2    -- Under 18: base 50
  ghci> fitnessScore 25 60 170 2    -- 18-30: base 80  
  ghci> fitnessScore 40 60 170 2    -- 31-50: base 70
  ghci> fitnessScore 55 60 170 2    -- 51-65: base 60
  ghci> fitnessScore 70 60 170 2    -- Over 65: base 50
  
  Test bonuses:
  ghci> fitnessScore 25 50 160 0    -- Low BMI, short, no exercise
  ghci> fitnessScore 25 50 185 5    -- Low BMI, tall, lots of exercise
  ghci> fitnessScore 25 90 170 0    -- High BMI (no BMI bonus)
  
  🧮 Manual calculation example:
  fitnessScore 25 70 175 3:
  • Base score: 80 (age 25 is 18-30 range)
  • BMI: 70 / (1.75²) ≈ 22.9 < 25 → +10 bonus  
  • Exercise: 3 * 2 = +6 bonus
  • Height: 175 < 180 → +0 bonus
  • Total: 80 + 10 + 6 + 0 = 96
  
  🎯 Key learning: Complex functions need systematic testing of each component!
-}

{-
  🐛 DEBUGGING TIPS:
  
  When your tests fail:
  1. Check the error message carefully - Haskell errors are informative
  2. Test smaller parts first (break complex functions into pieces)  
  3. Use :type in GHCi to check function types
  4. Print intermediate values using 'where' clauses temporarily
  5. Double-check your mathematical formulas
  6. Make sure your guards cover all cases (use 'otherwise')
  
  Common issues:
  • Integer vs Float division (use / not `div`)
  • Missing parentheses (function application has high precedence)
  • Guard conditions that don't cover all cases
  • Type mismatches (Int vs Float)
-}

{-
  🎓 WHAT YOU'VE LEARNED:
  
  By testing your own code, you've practiced:
  • Using GHCi effectively for interactive development
  • Thinking about edge cases and boundary conditions  
  • Verifying mathematical calculations
  • Understanding function composition and partial application
  • Debugging skills and systematic problem-solving
  
  These testing skills will serve you well in all future programming!
  Keep practicing this approach in later exercises.
-}

-- 💡 BONUS: Want to see automated testing? Check out Haskell testing libraries like:
-- • HUnit (for unit testing)
-- • QuickCheck (for property-based testing)  
-- • Hspec (for behavior-driven development)
-- But for learning, manual testing in GHCi is often better!
