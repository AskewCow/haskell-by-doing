# 🐏 Haskell Syntax Cheat Sheet

> 📚 **Reference Guide** - Organized by learning progression from basic to advanced topics

> 💡 **Tip:** Right-click this file and choose **"Open Preview"** (or press `Ctrl+Shift+V` / `Cmd+Shift+V`) for a better reading experience!

---

## 📖 Table of Contents

1. 🔤 Fundamentals
2. 🧮 Expressions & Types
3. ⚡ Functions
4. 📝 Lists & Tuples
5. 🎯 Pattern Matching
6. 🔄 Recursion
7. 🎭 Higher-Order Functions
8. 🏷️ Typeclasses
9. 🏗️ Defining Your Own Types
10. 📦 Modules & Imports
11. 🌍 IO & Real Programs
12. 🔮 Functors, Applicatives & Monads
13. 🚀 Advanced Topics
14. 🎯 Quick Reference
15. 📚 Additional Resources

---

## 🔤 Fundamentals

### 📄 Basic File Structure

```haskell
-- Comments start with --
{- Multi-line
   comments -}

module Main where

main :: IO ()
main = putStrLn "Hello, World!"
```

### 💬 Basic I/O

```haskell
putStrLn "Hello"           -- Print with newline
print 42                   -- Print any showable value
getLine                    -- Read a line from input
```

### 🔢 Basic Values

```haskell
x = 42                     -- Integer
y = 3.14                   -- Float
name = "Alice"             -- String
isTrue = True              -- Boolean
```

---

## 🧮 Expressions & Types

### 🎯 Type Annotations

```haskell
x :: Int
x = 5

name :: String
name = "Bob"

isValid :: Bool
isValid = True
```

### 🔢 Numeric Types

```haskell
int :: Int                 -- Fixed-precision integer
integer :: Integer         -- Arbitrary-precision integer
float :: Float             -- Single-precision floating point
double :: Double           -- Double-precision floating point
```

### ➕ Arithmetic Operators

```haskell
5 + 3                      -- Addition: 8
5 - 3                      -- Subtraction: 2
5 * 3                      -- Multiplication: 15
5 / 3                      -- Division: 1.666...
5 `div` 3                  -- Integer division: 1
5 `mod` 3                  -- Modulo: 2
5 ^ 3                      -- Exponentiation: 125
```

### 🔍 Comparison Operators

```haskell
5 == 3                     -- Equal: False
5 /= 3                     -- Not equal: True
5 > 3                      -- Greater: True
5 < 3                      -- Less: False
5 >= 3                     -- Greater or equal: True
5 <= 3                     -- Less or equal: False
```

### 🧠 Logical Operators

```haskell
True && False              -- AND: False
True || False              -- OR: True
not True                   -- NOT: False
```

---

## ⚡ Functions

### 📝 Function Definition

```haskell
-- Simple function
double x = x * 2

-- With type signature
double :: Int -> Int
double x = x * 2

-- Multiple parameters
add :: Int -> Int -> Int
add x y = x + y
```

### 🔀 Function Application

```haskell
double 5                   -- Result: 10
add 3 4                    -- Result: 7
add (double 3) 4           -- Result: 10 (parentheses for precedence)
```

### 🎨 Infix Functions

```haskell
5 `add` 3                  -- Infix notation: 8
(+) 5 3                    -- Prefix notation: 8
```

### 🌟 Lambda Functions

```haskell
\x -> x * 2                -- Anonymous function
(\x y -> x + y) 3 4        -- Multi-parameter lambda: 7
```

---

## 📝 Lists & Tuples

### 📋 Lists

```haskell
-- List creation
numbers = [1, 2, 3, 4, 5]
empty = []
range = [1..10]            -- [1,2,3,4,5,6,7,8,9,10]
infinite = [1..]           -- Infinite list

-- List operations
head [1,2,3]               -- First element: 1
tail [1,2,3]               -- All but first: [2,3]
init [1,2,3]               -- All but last: [1,2]
last [1,2,3]               -- Last element: 3
length [1,2,3]             -- Length: 3
null []                    -- Check if empty: True

-- List construction
1 : [2,3]                  -- Cons operator: [1,2,3]
[1,2] ++ [3,4]             -- Concatenation: [1,2,3,4]
```

### 📦 Tuples

```haskell
-- Tuple creation
point = (3, 4)             -- Pair
triple = (1, "hello", True) -- Triple

-- Tuple access
fst (3, 4)                 -- First element: 3
snd (3, 4)                 -- Second element: 4
```

### 🔧 List Comprehensions

```haskell
[x * 2 | x <- [1..5]]                    -- [2,4,6,8,10]
[x | x <- [1..10], x `mod` 2 == 0]       -- [2,4,6,8,10]
[(x,y) | x <- [1,2], y <- ['a','b']]     -- [(1,'a'),(1,'b'),(2,'a'),(2,'b')]
```

---

## 🎯 Pattern Matching

### 🔍 Basic Patterns

```haskell
-- Function with pattern matching
factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n - 1)

-- List patterns
isEmpty :: [a] -> Bool
isEmpty [] = True
isEmpty _  = False

listLength :: [a] -> Int
listLength []     = 0
listLength (_:xs) = 1 + listLength xs
```

### 🎭 Tuple Patterns

```haskell
addPair :: (Int, Int) -> Int
addPair (x, y) = x + y

first3 :: (a, b, c) -> a
first3 (x, _, _) = x
```

### 🌟 Guards

```haskell
grade :: Int -> String
grade score
  | score >= 90 = "A"
  | score >= 80 = "B"
  | score >= 70 = "C"
  | score >= 60 = "D"
  | otherwise   = "F"
```

### 🔄 Case Expressions

```haskell
describeList :: [a] -> String
describeList xs = case xs of
  []  -> "Empty list"
  [x] -> "Singleton list"
  _   -> "Longer list"
```

---

## 🔄 Recursion

### 🔁 Basic Recursion Patterns

```haskell
-- Countdown
countdown :: Int -> [Int]
countdown 0 = [0]
countdown n = n : countdown (n - 1)

-- Sum of list
sumList :: [Int] -> Int
sumList []     = 0
sumList (x:xs) = x + sumList xs

-- Map implementation
myMap :: (a -> b) -> [a] -> [b]
myMap _ []     = []
myMap f (x:xs) = f x : myMap f xs
```

### 🏃‍♂️ Tail Recursion

```haskell
-- Tail recursive factorial
factorialTR :: Int -> Int
factorialTR n = factHelper n 1
  where
    factHelper 0 acc = acc
    factHelper n acc = factHelper (n - 1) (n * acc)
```

---

## 🎭 Higher-Order Functions

### 🗺️ Map, Filter, Fold

```haskell
-- Map: apply function to each element
map (*2) [1,2,3]           -- [2,4,6]
map show [1,2,3]           -- ["1","2","3"]

-- Filter: keep elements that satisfy predicate
filter even [1,2,3,4,5]    -- [2,4]
filter (> 3) [1,2,3,4,5]   -- [4,5]

-- Fold: reduce list to single value
foldl (+) 0 [1,2,3,4]      -- Left fold: 10
foldr (*) 1 [1,2,3,4]      -- Right fold: 24
```

### 🔧 Function Composition

```haskell
-- Composition operator (.)
(.) :: (b -> c) -> (a -> b) -> a -> c
f . g = \x -> f (g x)

-- Examples
doubleAndSquare = (^2) . (*2)
doubleAndSquare 3          -- Result: 36

-- Application operator ($)
($) :: (a -> b) -> a -> b
f $ x = f x

map ($ 3) [(+1), (*2), (^2)]  -- [4,6,9]
```

### ⚡ Partial Application & Currying

```haskell
add :: Int -> Int -> Int
add x y = x + y

add5 = add 5               -- Partially applied function
add5 3                     -- Result: 8

-- Sections
(+1)                       -- Partially applied operator
(2*)                       -- Multiply by 2
(/2)                       -- Divide by 2
```

---

## 🏷️ Typeclasses

### 📚 Common Typeclasses

```haskell
-- Eq: equality comparison
class Eq a where
  (==) :: a -> a -> Bool
  (/=) :: a -> a -> Bool

-- Ord: ordering
class Eq a => Ord a where
  compare :: a -> a -> Ordering
  (<), (<=), (>), (>=) :: a -> a -> Bool

-- Show: convert to string
class Show a where
  show :: a -> String

-- Read: parse from string
class Read a where
  read :: String -> a
```

### 🎯 Using Typeclasses

```haskell
-- Type constraints
equal :: Eq a => a -> a -> Bool
equal x y = x == y

maximum' :: Ord a => [a] -> a
maximum' [x] = x
maximum' (x:xs) = max x (maximum' xs)

-- Multiple constraints
showAndCompare :: (Show a, Ord a) => a -> a -> String
showAndCompare x y = show x ++ " compared to " ++ show y
```

---

## 🏗️ Defining Your Own Types

### 🎨 Data Types

```haskell
-- Simple data type
data Color = Red | Green | Blue

-- Parameterized data type
data Maybe a = Nothing | Just a

-- Record syntax
data Person = Person
  { name :: String
  , age  :: Int
  , email :: String
  }
```

### 🏷️ Type Aliases

```haskell
type String = [Char]
type Point = (Int, Int)
type Name = String
```

### 🔄 Recursive Data Types

```haskell
-- Binary tree
data Tree a = Empty | Node a (Tree a) (Tree a)

-- List implementation
data List a = Nil | Cons a (List a)
```

### 🎭 Deriving

```haskell
data Color = Red | Green | Blue
  deriving (Show, Eq, Ord)

data Person = Person String Int
  deriving (Show, Eq)
```

---

## 📦 Modules & Imports

### 📥 Importing Modules

```haskell
import Data.List                    -- Import everything
import Data.List (sort, nub)        -- Import specific functions
import Data.List hiding (head)      -- Import everything except head
import qualified Data.Map as Map    -- Qualified import
import Data.Set (Set)               -- Import type
```

### 📤 Exporting from Modules

```haskell
module MyModule
  ( functionA
  , functionB
  , TypeA(..)        -- Export type and all constructors
  , TypeB(Cons1)     -- Export type and specific constructor
  ) where

functionA = ...
functionB = ...
```

### 🏠 Module Structure

```haskell
-- File: Data/MyModule.hs
module Data.MyModule where

-- implementations...
```

---

## 🌍 IO & Real Programs

### 💬 Basic IO Actions

```haskell
-- Input/Output
putStrLn :: String -> IO ()         -- Print line
putStr :: String -> IO ()           -- Print without newline
print :: Show a => a -> IO ()       -- Print any showable value
getLine :: IO String                -- Read line
getChar :: IO Char                  -- Read character
```

### 🔗 Combining IO Actions

```haskell
-- do notation
main :: IO ()
main = do
  putStrLn "What's your name?"
  name <- getLine
  putStrLn ("Hello, " ++ name ++ "!")

-- Using >>= (bind)
main = putStrLn "What's your name?" >>
       getLine >>= \name ->
       putStrLn ("Hello, " ++ name ++ "!")
```

### 📁 File Operations

```haskell
import System.IO

-- Reading files
content <- readFile "input.txt"

-- Writing files
writeFile "output.txt" "Hello, file!"

-- Appending to files
appendFile "log.txt" "New entry\n"
```

---

## 🔮 Functors, Applicatives & Monads

### 🗺️ Functor

```haskell
class Functor f where
  fmap :: (a -> b) -> f a -> f b

-- Examples
fmap (+1) [1,2,3]          -- [2,3,4]
fmap (*2) (Just 5)         -- Just 10
fmap show Nothing          -- Nothing

-- Infix operator
(+1) <$> [1,2,3]           -- [2,3,4]
```

### ⚡ Applicative

```haskell
class Functor f => Applicative f where
  pure :: a -> f a
  (<*>) :: f (a -> b) -> f a -> f b

-- Examples
pure (+) <*> Just 3 <*> Just 4     -- Just 7
[(+1), (*2)] <*> [1,2,3]           -- [2,3,4,2,4,6]
```

### 🔗 Monad

```haskell
class Applicative m => Monad m where
  (>>=) :: m a -> (a -> m b) -> m b
  return :: a -> m a

-- do notation sugar
do x <- action1
   y <- action2 x
   return (x + y)

-- Equivalent to:
action1 >>= \x ->
action2 x >>= \y ->
return (x + y)
```

### 🎯 Common Monads

```haskell
-- Maybe monad
safeDivide :: Double -> Double -> Maybe Double
safeDivide _ 0 = Nothing
safeDivide x y = Just (x / y)

-- List monad
pairs = do
  x <- [1,2,3]
  y <- [4,5,6]
  return (x, y)

-- IO monad (already covered above)
```

---

## 🚀 Advanced Topics

### 🎭 Type Families

```haskell
type family Element c
type instance Element [a] = a
type instance Element (Set a) = a
```

### 🔧 GADTs (Generalized Algebraic Data Types)

```haskell
{-# LANGUAGE GADTs #-}

data Expr a where
  I :: Int -> Expr Int
  B :: Bool -> Expr Bool
  Add :: Expr Int -> Expr Int -> Expr Int
  Eq :: Expr Int -> Expr Int -> Expr Bool
```

### ⚡ Template Haskell

```haskell
{-# LANGUAGE TemplateHaskell #-}
import Language.Haskell.TH

-- Generate getter/setter functions
makeLenses ''Person
```

### 🧠 Lazy Evaluation

```haskell
-- Infinite lists
ones = 1 : ones               -- [1,1,1,1,...]
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

-- Only compute what's needed
take 5 fibs                   -- [0,1,1,2,3]
```

---

## 🎯 Quick Reference

### 🔧 Useful Functions

```haskell
-- List functions
head, tail, init, last, length, null, reverse
(++), (:), take, drop, splitAt, zip, unzip

-- Higher-order functions
map, filter, foldl, foldr, scanl, scanr
any, all, takeWhile, dropWhile

-- Maybe functions
maybe, fromMaybe, isJust, isNothing

-- Tuple functions
fst, snd, curry, uncurry

-- Type conversion
show, read, fromIntegral, round, floor, ceiling
```

### 🎨 Operators Precedence (High to Low)

```haskell
-- 9: Function application (highest)
-- 8: ^, ^^, **
-- 7: *, /, `div`, `mod`, `rem`, `quot`
-- 6: +, -
-- 5: :, ++
-- 4: ==, /=, <, <=, >, >=, `elem`, `notElem`
-- 3: &&
-- 2: ||
-- 1: >>, >>=
-- 0: $, $!, `seq` (lowest)
```

---

## 📚 Additional Resources

-   🌐 [Hoogle](https://hoogle.haskell.org/) - Function search
-   📖 [Learn You a Haskell](http://learnyouahaskell.com/)
-   🏠 [Hackage](https://hackage.haskell.org/) - Package repository
-   💬 [r/haskell](https://reddit.com/r/haskell) - Community
-   🎓 [Haskell Wiki](https://wiki.haskell.org/)

---

> 🎉 **Happy Haskelling!** Remember: practice makes perfect, and Haskell's elegance will grow on you! 🌱
