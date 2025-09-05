# 📦 04 – Lists & Tuples

In the last topic, you learned how to **build and apply functions**.
Now let's see how Haskell lets us work with many values at once: **lists and tuples**.

These are the **fundamental data structures** in Haskell. You'll use them everywhere.

> **Note:** If you know Python/JavaScript lists and tuples, don't assume they work the same here - Haskell's are stricter with types and immutable!

---

## 📋 Lists: The Workhorse of Haskell

A **list** is just an ordered collection of values of the _same type_.

```haskell
numbers :: [Int]
numbers = [1, 2, 3, 4]

names :: [String]
names   = ["Alice", "Bob", "Charlie"]

grades :: [Double]
grades = [85.5, 92.0, 78.5]
```

-   `numbers` is a list of `Int`
-   `names` is a list of `String`
-   `grades` is a list of `Double`

Even an empty list can have a type:

```haskell
empty :: [Int]
empty = []

emptyNames :: [String]
emptyNames = []
```

⚠️ **Important Rule**: All elements must be the same type.
This won't work:

```haskell
[1, "two", 3]   -- ❌ error, because Int ≠ String
```

### 🔍 Understanding List Types

When you see `[Int]`, read it as "list of Int".
The type tells you what's inside:

```haskell
[Int]     -- list of integers
[String]  -- list of strings
[Bool]    -- list of booleans
[[Int]]   -- list of lists of integers!
```

---

## ⚡ Common List Functions

Let's explore some list methods. Here is our sample list:

```haskell
nums :: [Int]
nums = [10, 20, 30, 40]
```

### Basic Access Functions

-   **First element**:

    ```haskell
    head :: [a] -> a        -- This is just the type signature (what the function expects)
    head nums               -- 10 (this is what you'll actually write)
    ```

-   **All but the first**:

    ```haskell
    tail :: [a] -> [a]
    tail nums      -- [20,30,40]
    ```

-   **Last element**:

    ```haskell
    last :: [a] -> a
    last nums      -- 40
    ```

-   **All but the last**:

    ```haskell
    init :: [a] -> [a]
    init nums      -- [10,20,30]
    ```

-   **Length**:
    ```haskell
    length :: [a] -> Int
    length nums    -- 4
    ```

### ⚠️ The Empty List Trap

Here's a crucial pitfall: if the list is **empty** some operations cause runtime errors!

```haskell
head []        -- ❌ Exception: empty list
tail []        -- ❌ Exception: empty list
last []        -- ❌ Exception: empty list
```

**The fix:** Always check if a list is empty with `null` before using `head`/`tail`/`last`, or use pattern matching (covered later).

```haskell
-- Check first with null:
if null myList
  then "List is empty!"
  else show (head myList)
```

---

### 📝 Building Bigger Lists

Lists aren't technically fixed-size, but they're **immutable** - you can't modify them, only create new, bigger ones:

```haskell
nums = [1, 2, 3]

-- Prepend (add to front) - FAST ⚡
newNums = 0 : nums          -- [0,1,2,3]

-- Concatenate lists - SLOWER for large lists ⚠️
moreNums = nums ++ [4, 5]   -- [1,2,3,4,5]
combined = [0, -1] ++ nums  -- [0,-1,1,2,3]

-- Original list is unchanged!
nums                        -- Still [1,2,3]
```

**Key points:**

-   **`:`** (pronounced "cons") adds ONE element to the front - very fast
-   **`:`** only works one way: `element : list`, NOT `list : element`
-   **`++`** joins two lists together - can be slow for long lists
-   The original list is **never modified** - you get a new list back

```haskell
-- ✅ This works: element : list
0 : [1,2,3]      -- [0,1,2,3]

-- ❌ This doesn't work: list : element
[1,2,3] : 0      -- Type error!

-- To add to the end, use ++
[1,2,3] ++ [0]   -- [1,2,3,0]
```

**Performance tip:** Prefer `:` over `++` when possible:

```haskell
-- ✅ Efficient: building a list by prepending
buildList = 1 : 2 : 3 : []  -- [1,2,3]

-- ❌ Less efficient: lots of concatenation
slowList = [1] ++ [2] ++ [3]  -- [1,2,3] but slower
```

---

### Applying Functions to Lists

You already know that functions are reusable pieces. Lists really shine when you combine them with **higher-order functions**.

These are **built-in functions** that come with Haskell - you don't need to define them:

#### 1️⃣ `map` - Transform Every Element

`map` takes a function and applies it to every element in a list:

```haskell
map (*2) [1,2,3]      -- [2,4,6]
```

What happened? `(*2)` means "multiply by 2", and `map` applied it to each element:

-   `1 * 2 = 2`
-   `2 * 2 = 4`
-   `3 * 2 = 6`

Another example:

```haskell
map show [1,2,3]      -- ["1","2","3"]
```

Here, `show` converts numbers to strings, so we get a list of strings.

#### 2️⃣ `filter` - Keep Only What You Want

`filter` keeps only elements that match a condition:

```haskell
filter even [1..10]   -- [2,4,6,8,10]
```

This keeps only the even numbers from 1 to 10.

```haskell
filter (> 5) [1..10]  -- [6,7,8,9,10]
```

This keeps only numbers greater than 5. The `(> 5)` is a function that asks "is this greater than 5?"

#### 3️⃣ `sum` - Add Everything Up

```haskell
sum [1,2,3,4]         -- 10
```

Pretty straightforward - adds all the numbers: 1 + 2 + 3 + 4 = 10.

#### 4️⃣ `foldr` - The General Pattern

`foldr` is like a Swiss Army knife - it can combine list elements in any way you want:

```haskell
foldr (+) 0 [1,2,3,4]     -- 10 (adds them up)
foldr (*) 1 [2,3,4]       -- 24 (multiplies them: 2*3*4)
foldr (++) "" ["Hi", " ", "there"]  -- "Hi there" (joins strings)
```

Think of it as: "take this list, and combine all elements using this operation, starting with this value."

Don't worry about `foldr` too much for now - `map`, `filter`, and `sum` are more important to understand first.

---

## 🔢 List Ranges

Writing `[1,2,3,4,5,6,7,8,9,10]` is tedious.
Haskell lets you generate ranges:

```haskell
[1..10]        -- [1,2,3,4,5,6,7,8,9,10]
[2,4..10]      -- [2,4,6,8,10] (step of 2)
[10,9..1]      -- [10,9,8,7,6,5,4,3,2,1] (counting down)
['a'..'e']     -- "abcde" (strings are lists of chars!)
```

### ♾️ Infinite Lists & Lazy Evaluation

Even **infinite lists**:

```haskell
[1..]                    -- [1,2,3,4,5,6,7,8,9,10,11,12...]
take 5 [1..]             -- [1,2,3,4,5]
take 10 (cycle [1,2,3])  -- [1,2,3,1,2,3,1,2,3,1]
repeat 'x'               -- ['x','x','x','x','x',...]
```

This works because of **lazy evaluation**, Haskell only generates values when you actually need them.

```haskell
-- This doesn't crash your computer!
evens = [2,4..]
first10Evens = take 10 evens  -- [2,4,6,8,10,12,14,16,18,20]
```

**Practical example**: Generate the first 100 prime numbers without calculating all primes:

```haskell
primes = sieve [2..]
  where
    sieve (p:xs) = p : sieve [x | x <- xs, x `mod` p /= 0]

first100Primes = take 100 primes
```

---

## 🎭 Tuples: Small Fixed Groups

A **tuple** groups a fixed number of values together, possibly of different types:

```haskell
pair :: (Int, String)
pair   = (1, "hello")

triple :: (Bool, Double, Char)
triple = (True, 3.14, 'x')

coordinate :: (Double, Double)
coordinate = (10.5, 20.3)
```

### Lists vs Tuples: When to Use Which?

| Lists                  | Tuples                       |
| ---------------------- | ---------------------------- |
| All elements same type | Elements can differ          |
| Any length (dynamic)   | Fixed length (static)        |
| Can grow/shrink        | Size decided at compile time |
| Use for collections    | Use for structured data      |
| `[1,2,3,4,5...]`       | `(name, age, isStudent)`     |

### Common Tuple Operations

For **pairs** (2-tuples), Haskell provides built-in functions:

```haskell
fst :: (a, b) -> a          -- Type signature: takes a tuple, returns first element
fst (10, "Hi")              -- 10

snd :: (a, b) -> b          -- Type signature: takes a tuple, returns second element
snd (10, "Hi")              -- "Hi"
```

**What's this `(a, b) -> a` syntax?**

-   `(a, b)` = a tuple containing any two types (could be Int, String, Bool, etc.)
-   `->` = "gives you back"
-   `a` = the first type from the tuple

The above is only shown so that you can see how it works behind the scenes. In reality, you would only have to write `fst (10, "Hi")`

For **larger tuples**, you'll use pattern matching (we will cover this properly in the next topic):

```haskell
getFirst :: (a, b, c) -> a
getFirst (x, _, _) = x

getMiddle :: (a, b, c) -> b
getMiddle (_, y, _) = y
```

### Practical Tuple Examples

**Return multiple values from functions**:

```haskell
divideWithRemainder :: Int -> Int -> (Int, Int)
divideWithRemainder x y = (x `div` y, x `mod` y)

result = divideWithRemainder 17 5   -- (3, 2)
quotient = fst result               -- 3
remainder = snd result              -- 2
```

**Coordinates and points**:

```haskell
type Point = (Double, Double)

distance :: Point -> Point -> Double
distance (x1, y1) (x2, y2) =
  sqrt ((x2 - x1)^2 + (y2 - y1)^2)

origin = (0, 0)
point = (3, 4)
dist = distance origin point  -- 5.0
```

---

## 🔄 Combining Lists and Tuples

Lists of tuples are very common:

```haskell
students :: [(String, Int, Double)]
students = [ ("Alice", 20, 85.5)
           , ("Bob", 19, 92.0)
           , ("Charlie", 21, 78.5)
           ]

-- Extract just the names
getNames :: [(String, Int, Double)] -> [String]
getNames students = map (\(name, _, _) -> name) students

names = getNames students  -- ["Alice", "Bob", "Charlie"]
```

**Key-value pairs** (like a simple dictionary):

```haskell
phoneBook :: [(String, String)]
phoneBook = [ ("Alice", "555-1234")
            , ("Bob", "555-5678")
            , ("Charlie", "555-9012")
            ]

-- Look up a phone number
lookup :: String -> [(String, String)] -> Maybe String
lookup name phoneBook =
  case filter (\(n, _) -> n == name) phoneBook of
    [] -> Nothing
    (_, phone):_ -> Just phone
```

> **Note:** The examples above use lambda functions `\(name, _, _) -> name` and pattern matching - these are more advanced concepts. Don't worry if they look confusing! Just know that you can work with lists of tuples, and we'll cover these techniques properly in later topics.

---

## 🎯 Common Patterns & Pitfalls

### ✅ Good Practices

1. **Use meaningful type aliases**:

    ```haskell
    type StudentRecord = (String, Int, Double)  -- name, age, grade
    type Point2D = (Double, Double)
    ```

2. **Prefer pattern matching over fst/snd**:

    ```haskell
    -- ❌ Hard to read
    processStudent student =
      show (fst student) ++ " is " ++ show (snd student) ++ " years old"

    -- ✅ Clear and readable
    processStudent (name, age, _) =
      name ++ " is " ++ show age ++ " years old"
    ```

3. **Use list comprehensions for complex filters**:
    ```haskell
    -- Find all even numbers between 1 and 20 that are divisible by 3
    result = [x | x <- [1..20], even x, x `mod` 3 == 0]  -- [6,12,18]
    ```

### ⚠️ Common Pitfalls

1. **Empty list operations**:

    ```haskell
    -- Always check for empty lists
    safeDivide :: [Double] -> Double
    safeDivide [] = 0
    safeDivide xs = sum xs / fromIntegral (length xs)
    ```

2. **Mixing up tuple order**:

    ```haskell
    type Point = (Double, Double)  -- Always document what each position means!
    -- Is this (x, y) or (y, x)? Be consistent!
    ```

3. **Off-by-one errors with indexing**:

    ```haskell
    (!!) :: [a] -> Int -> a     -- The !! operator accesses elements by index
    [10,20,30] !! 0   -- 10 (first element - indexing starts at 0)
    [10,20,30] !! 1   -- 20 (second element)
    [10,20,30] !! 3   -- ❌ Index out of bounds! (list only has 3 elements: 0,1,2)
    ```

---

## 🔗 What's Next?

You should now understand:

-   ✅ How to create and manipulate lists safely
-   ✅ The power of ranges and lazy evaluation
-   ✅ When to use tuples vs lists
-   ✅ How to combine lists and tuples effectively
-   ✅ Common pitfalls and how to avoid them

**Now practice!** Complete the exercises in [`exercise.hs`](../exercise.hs) to reinforce what you've learned with hands-on coding.

**Next up**: **Pattern Matching**- the elegant way to take apart lists, tuples, and other data structures safely. This will make working with these structures much more powerful and readable!

---

## 📚 Quick Reference

_Adapted from the [Haskell Syntax Cheatsheet](../../HASKELL-SYNTAX-CHEATSHEET.md)_

```haskell
-- List basics
[]              -- empty list
[1,2,3]         -- list literal
1:[2,3]         -- cons operator (prepend)
[1,2] ++ [3,4]  -- concatenation: [1,2,3,4]

-- List functions
head [1,2,3]    -- 1
tail [1,2,3]    -- [2,3]
length [1,2,3]  -- 3
null []         -- True
reverse [1,2,3] -- [3,2,1]

-- Ranges
[1..10]         -- [1,2,3,4,5,6,7,8,9,10]
[2,4..10]       -- [2,4,6,8,10]
take 5 [1..]    -- [1,2,3,4,5]

-- Tuples
(1, "hello")    -- pair
(1, 2, 3)       -- triple
fst (1, 2)      -- 1
snd (1, 2)      -- 2

-- Higher-order functions
map f [1,2,3]       -- apply f to each element
filter p [1,2,3]    -- keep elements where p is True
foldr f z [1,2,3]   -- reduce list with function f
```
