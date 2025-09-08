-- 📝 05 – Pattern Matching Solutions

{-
Solutions to Pattern Matching exercises.

These demonstrate the power and elegance of pattern matching in Haskell.
Study how each solution handles different cases safely and clearly.
-}

-- 1️⃣ Exercise 1: Factorial with Pattern Matching
-- Define factorial using pattern matching (not guards!)
-- Remember: factorial(0) = 1, factorial(n) = n * factorial(n-1)
-- Examples: factorial 5 = 120, factorial 0 = 1
factorial :: Int -> Int
factorial 0 = 1                    -- Base case: 0! = 1
factorial n = n * factorial (n-1)  -- Recursive case: n! = n * (n-1)!

-- 2️⃣ Exercise 2: Describe List Structure
-- Write a function that describes what kind of list it receives
-- Examples:
--   describeList [] = "empty"
--   describeList [1] = "singleton"  
--   describeList [1,2] = "pair"
--   describeList [1,2,3,4,5] = "longer list"
describeList :: [a] -> String
describeList [] = "empty"           -- Empty list
describeList [_] = "singleton"      -- Single element (we don't care what it is)
describeList [_, _] = "pair"        -- Exactly two elements
describeList _ = "longer list"      -- Everything else (3 or more elements)

-- 3️⃣ Exercise 3: Safe List Operations
-- Write safe versions of head and tail that return Maybe values
-- Examples:
--   safeHead [] = Nothing, safeHead [1,2,3] = Just 1
--   safeTail [] = Nothing, safeTail [1,2,3] = Just [2,3]
safeHead :: [a] -> Maybe a
safeHead [] = Nothing       -- Empty list has no head
safeHead (x:_) = Just x     -- Non-empty list: return the first element

safeTail :: [a] -> Maybe [a]
safeTail [] = Nothing       -- Empty list has no tail
safeTail (_:xs) = Just xs   -- Non-empty list: return everything after the first

-- 4️⃣ Exercise 4: List Length (Recursive)
-- Calculate the length of a list using pattern matching and recursion
-- Examples: myLength [] = 0, myLength [1,2,3] = 3
myLength :: [a] -> Int
myLength [] = 0                    -- Empty list has length 0
myLength (_:xs) = 1 + myLength xs  -- Length is 1 + length of remaining elements

-- 5️⃣ Exercise 5: Sum of Numbers
-- Sum all numbers in a list using pattern matching
-- Examples: mySum [] = 0, mySum [1,2,3] = 6
mySum :: [Int] -> Int
mySum [] = 0                  -- Empty list sums to 0
mySum (x:xs) = x + mySum xs   -- Add first element to sum of the rest

-- 6️⃣ Exercise 6: Pattern Matching with Tuples
-- Extract the first element from a triple (3-tuple)
-- Example: firstOfThree ("hello", 42, True) = "hello"
firstOfThree :: (a, b, c) -> a
firstOfThree (x, _, _) = x    -- Extract first element, ignore the other two

-- 7️⃣ Exercise 7: Combining Pattern Matching and Guards
-- Classify a list based on its length AND first element (if it exists)
-- Use pattern matching to handle structure, guards for conditions
-- Examples:
--   classifyList [] = "empty list"
--   classifyList [x] where x > 0 = "positive singleton" 
--   classifyList [x] where x <= 0 = "non-positive singleton"
--   classifyList (x:xs) where length xs >= 3 = "long list"
--   classifyList (x:xs) = "short list"
classifyList :: [Int] -> String
classifyList [] = "empty list"              -- Pattern matching: empty list
classifyList [x]                            -- Pattern matching: single element
  | x > 0     = "positive singleton"        -- Guard: check if positive
  | otherwise = "non-positive singleton"    -- Guard: everything else
classifyList (x:xs)                         -- Pattern matching: multiple elements
  | length xs >= 3 = "long list"           -- Guard: check if long (4+ elements total)
  | otherwise      = "short list"           -- Guard: short (2-3 elements total)

-- 8️⃣ Exercise 8: Advanced - Remove All Occurrences
-- Remove all instances of a given value from a list
-- Examples: 
--   removeAll 3 [1,3,2,3,4] = [1,2,4]
--   removeAll 'a' "banana" = "bnn" 
removeAll :: Eq a => a -> [a] -> [a]
removeAll _ [] = []                            -- Nothing to remove from empty list
removeAll target (x:xs)
  | x == target = removeAll target xs          -- Skip this element (it matches)
  | otherwise   = x : removeAll target xs      -- Keep this element and continue

-- 🎯 Bonus Exercise: List Reversal
-- Reverse a list using pattern matching and recursion
-- Example: myReverse [1,2,3] = [3,2,1]
-- Hint: You might need a helper function with an accumulator
myReverse :: [a] -> [a]
myReverse xs = reverseHelper xs []
  where
    reverseHelper :: [a] -> [a] -> [a]
    reverseHelper [] acc = acc                             -- Base case: return accumulator
    reverseHelper (y:ys) acc = reverseHelper ys (y:acc)    -- Move element to front of accumulator

-- Alternative solution without helper function (less efficient):
-- myReverse [] = []
-- myReverse (x:xs) = myReverse xs ++ [x]

{-
📖 SOLUTION EXPLANATIONS:

1. **factorial**: Classic recursive pattern - base case for 0, recursive case for n.

2. **describeList**: Uses increasingly specific patterns. Order matters here!
   [] matches empty, [_] matches single element, [_,_] matches pairs, _ catches the rest.

3. **safeHead/safeTail**: The Maybe type makes these functions total (never crash).
   Empty list → Nothing, non-empty → Just the result.

4. **myLength**: Classic recursive pattern. Each element adds 1 to the length of the rest.

5. **mySum**: Similar to length, but adds the actual values instead of counting.

6. **firstOfThree**: Simple tuple destructuring. The _ shows we ignore unused parts.

7. **classifyList**: Combines pattern matching (for structure) with guards (for conditions).
   This shows the power of using both techniques together.

8. **removeAll**: Recursive filtering. Uses guards to decide whether to keep each element.

9. **myReverse**: Uses an accumulator pattern for efficiency. Each element gets prepended
   to the accumulator, which naturally reverses the order.

🎯 KEY PATTERNS TO REMEMBER:
- Always handle the base case (usually empty list or 0)
- Use _ for values you don't need
- Pattern order matters - most specific first
- Combine patterns with guards for maximum power
- The (x:xs) pattern is your best friend for list recursion!
-}
