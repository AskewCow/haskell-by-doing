# 🐏 03 – Functions

Welcome to **Functions**! ⭐
In Haskell, functions are the **heart** of the language. You've already seen expressions and types, now you'll learn how to wrap them into reusable building blocks.

> 💡 **Learning Tip:** This course includes a built-in notes system! Keep track of concepts you struggle with in one convenient place instead of re-reading entire lessons. Check out the `notes/` folder to reinforce your learning and review key insights anytime.

---

## 📝 Function Definitions

A **function** is just giving a name to an expression (with inputs).
General form:

```haskell
functionName arg1 arg2 ... = expression
```

Example:

```haskell
square x = x * x
```

-   `square` is the function's name
-   `x` is the parameter (input)
-   `x * x` is the body (output expression)

Try it in GHCi (define it first, then use it):

```haskell
ghci> square x = x * x
ghci> square 5
25
```

---

## 🏷️ Type Signatures

Every function has a **type**.
We write it above the definition using `::` ("has type").

```haskell
square :: Int -> Int
square x = x * x
```

📝 **Simplified**: The rightmost type is always the output. Everything before it are inputs.

-   `Input Type -> Output Type`

💡 Just like before, Haskell can **infer** this type for you, but writing it makes your code more readable and less error-prone.

---

## ➡️ Multiple Arguments & Currying

In Haskell, a function always takes just **one** argument.
So how do we handle multiple arguments? → We chain functions together.

### Step 1: The Normal Definition

```haskell
add :: Int -> Int -> Int
add x y = x + y
```

Looks like `add` takes two numbers and returns one.

But actually…

### Step 2: What It Really Means

```haskell
add :: Int -> (Int -> Int)
```

Read as:

-   "`add` takes an `Int`"
-   "and returns a function that takes another `Int`"
-   "and that second function returns an `Int`"

So `Int -> Int -> Int` is really shorthand for `Int -> (Int -> Int)`.

### Step 3: Applying Arguments

When you call:

```haskell
add 3 4
```

This is actually evaluated as:

```haskell
(add 3) 4
```

-   `add 3` → produces a new function of type `Int -> Int` ("add three to whatever you give me").
-   Then `(... 4)` applies that new function to `4`.

### Step 4: Partial Application

Since functions take arguments one at a time, you don't have to give them all at once:

```haskell
add 3       -- produces a function Int -> Int
(add 3) 4   -- applies it to 4, result = 7
```

You can save the partially applied function:

```haskell
inc :: Int -> Int
inc = add 1

inc 5   -- 6
inc 100 -- 101
```

Another one:

```haskell
addTen = add 10
addTen 25
-- 35
```

✅ **Key takeaway:**
Currying = turning a multi-argument function into a series of single-argument functions.
That's why every function in Haskell is technically a one-argument function; multi-argument syntax is just a convenient shorthand that Haskell provides to make code easier to read and write.

> 💭 **Don't worry if this feels confusing at first!** Currying is one of the trickier concepts in functional programming. It takes time and practice to really "click." The exercises will give you hands-on experience with these ideas, making them much clearer.

---

## ⚖️ Function Application Precedence

Function calls in Haskell **don't** use parentheses or commas.
Instead, you **separate arguments with spaces** and place them directly after the function name:

```haskell
-- Haskell
f x y
```

Here's what's happening step by step:

-   `f` is a function that takes two arguments
-   `x` and `y` are the arguments (values or variables)
-   Due to currying, `f x y` is evaluated as `(f x) y`
-   First: `f x` produces a new function (remember: `f` takes one argument at a time)
-   Then: that new function is applied to `y` to get the final result

**Example:**

```haskell
add 3 5
-- Step 1: (add 3) creates a function "add 3 to whatever comes next"
-- Step 2: (add 3) 5 applies that function to 5, giving us 8
```

⚠️ Important rules:

1. **Function application has highest precedence** (binds tighter than operators).

    ```haskell
    square 3 + 4
    -- means (square 3) + 4, not square (3 + 4)
    ```

2. **No commas between arguments**.

    ```haskell
    max 5 10
    -- ✅ correct

    max(5, 10)
    -- ❌ error
    ```

3. **Use parentheses to group expressions**.

    ```haskell
    square (3 + 4)
    -- 49
    ```

### More Precedence Examples

```haskell
-- These are all different:
f g h x    -- means ((f g) h) x
f (g h x)  -- means f (g (h x))
f g (h x)  -- means (f g) (h x)
```

---

## 🛡️ Guards: Better Conditionals

**Guards** are Haskell's way of choosing between different expressions based on conditions. Think of them as a cleaner alternative to multiple `if-then-else` statements.

### How Guards Work

Instead of writing messy nested conditionals:

```haskell
-- Ugly nested if-then-else
absoluteValue n = if n >= 0 then n else (-n)
```

You can use guards with the `|` symbol (pipe):

```haskell
-- Clean guards syntax
absoluteValue :: Int -> Int
absoluteValue n
  | n >= 0    = n      -- if n >= 0, return n
  | otherwise = -n     -- otherwise, return -n
```

### Guard Syntax Rules

1. **Each guard starts with `|`** followed by a boolean condition
2. **Each guard ends with `=`** followed by the result expression
3. **Guards are checked top to bottom** until one matches
4. **`otherwise`** is just another name for `True` (catches any remaining cases)

### More Complex Example

```haskell
gradeToLetter :: Int -> String
gradeToLetter score
  | score >= 90 = "A"    -- if score is 90 or higher
  | score >= 80 = "B"    -- else if score is 80-89
  | score >= 70 = "C"    -- else if score is 70-79
  | score >= 60 = "D"    -- else if score is 60-69
  | otherwise   = "F"    -- else (anything below 60)
```

---

## 🏠 Local Definitions: Where & Let

Sometimes your function needs to calculate intermediate values that you'll use multiple times or that make your code clearer. These are called **helper values** - they're not the final result, but they help you build toward it.

**Helper values** let you:

-   Break complex calculations into smaller, named pieces
-   Avoid repeating the same computation
-   Make your code more readable

Haskell gives you two ways to define these local helper values: `where` and `let`.

### Using `where`

With `where`, you write your main expression first, then define the helper values afterward:

```haskell
hypotenuse :: Float -> Float -> Float
hypotenuse a b = sqrt total      -- main expression uses 'total'
  where
    total = a*a + b*b           -- helper value defined here

bmiTell :: Float -> Float -> String
bmiTell weight height =
    if bmi < 18.5               -- main expression uses 'bmi'
    then "Underweight"
    else if bmi < 25.0
    then "Normal"
    else "Overweight"
  where
    bmi = weight / height^2     -- helper value calculated once
```

### Using `let`

With `let`, you define the helper values first, then use them in the main expression after `in`:

```haskell
hypotenuse :: Float -> Float -> Float
hypotenuse a b =
    let total = a*a + b*b       -- helper values defined first
    in sqrt total               -- main expression after 'in'

cylinderArea :: Float -> Float -> Float
cylinderArea r h =
    let sideArea = 2 * pi * r * h    -- multiple helper values
        topArea = pi * r^2
    in sideArea + 2 * topArea        -- main expression uses both
```

### When to Use Which?

-   **`where`**: When you want to focus on the main logic first, then explain the details
-   **`let`**: When you want to set up your calculations first, then show the final step

Both do the same thing - they just read differently!

---

## 🔗 Function Composition

**Function composition** is a way to create new functions by chaining existing functions together. Think of it like connecting pipes; the output of one function becomes the input of the next.

### Why Compose Functions?

Instead of writing deeply nested function calls that are hard to read:

```haskell
result = square (addOne (multiply 2 x))  -- hard to follow the flow
```

You can create a clear pipeline of transformations:

```haskell
transform = square . addOne . multiply 2
result = transform x                     -- much cleaner!
```

### How the `(.)` Operator Works

The composition operator `(.)` has this type:

```haskell
(.) :: (b -> c) -> (a -> b) -> a -> c
```

This means: "give me a function from `b` to `c`, and a function from `a` to `b`, and I'll give you a function from `a` to `c`."

**Key insight**: The functions are applied **right to left** (like mathematical function composition).

### Step-by-Step Example

```haskell
addOne x = x + 1
square x = x * x

-- Create a composed function
addThenSquare = square . addOne
-- This is equivalent to: addThenSquare x = square (addOne x)

-- Usage
addThenSquare 3
-- Step 1: addOne 3 = 4
-- Step 2: square 4 = 16
-- Result: 16
```

### Chaining Multiple Functions

```haskell
double x = x * 2
addOne x = x + 1
square x = x * x

-- Chain them together (read right to left)
transform = square . addOne . double
-- This means: double first, then addOne, then square

-- Usage
transform 3
-- Step 1: double 3 = 6
-- Step 2: addOne 6 = 7
-- Step 3: square 7 = 49
-- Result: 49
```

**Benefits:**

-   **Readability**: Clear pipeline of transformations
-   **Reusability**: Compose functions to create new, specialized functions
-   **No intermediate variables**: No need to store temporary results

---

## 🔄 Higher-Order Functions Preview

Functions can **take other functions** as arguments:

```haskell
applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

-- Usage
applyTwice square 3   -- 81 (square(square(3)))
applyTwice (+1) 5     -- 7  ((+1)((+1)(5)))
```

> 💡 Don't worry if this seems complex, we'll cover higher-order functions in detail later!

---

## ⚠️ Common Pitfalls

### Function Application Precedence

```haskell
sqrt a * a + b * b
-- actually means (sqrt a) * a + b * b
-- ❌ wrong for hypotenuse!

sqrt (a*a + b*b)
-- ✅ correct
```

### Arrow Type Confusion

```haskell
-- Don't think -> means "returns"
Int -> Int -> Int
-- More precise: "takes an Int, then takes another Int, produces an Int"
-- Really means: Int -> (Int -> Int)
```

### Guard vs If-Then-Else

```haskell
-- Avoid deeply nested if-then-else:
if score >= 90 then "A" else if score >= 80 then "B" else ...

-- Use guards instead:
gradeToLetter score
  | score >= 90 = "A"
  | score >= 80 = "B"
  -- much cleaner!
```

---

## 🔗 What's Next?

You will reinforce know how to write and use functions with:

-   ✅ Type signatures and currying
-   ✅ Guards for clean conditionals
-   ✅ Local definitions with where/let
-   ✅ Function composition

👉 **Head over to** [`exercise.hs`](../exercise.hs) and practice these concepts!
You'll also learn how to test your own code using instructions in [`tester.hs`](../tester.hs) - an essential programming skill.

Next, we'll explore **Lists & Tuples**, Haskell's fundamental data structures that work beautifully with functions.

---

## 💖 Enjoying This Course?

If you've made it this far and these lessons are helping you learn Haskell, I'd be incredibly grateful if you could show some love! It may seem small to you, but it truly means the world to me and helps others discover this resource.

<img src="https://img.shields.io/badge/⭐-Star%20this%20repo%20if%20it%20helped%20you!-FFD700?style=for-the-badge&logo=github&logoColor=white" alt="Star this repo"/>

<br/>

<a href="https://github.com/AskewCow/haskell-by-doing/stargazers">
    <img src="https://img.shields.io/github/stars/AskewCow/haskell-by-doing?style=social&label=Star" alt="GitHub stars"/>
</a>

Thank you for being part of this learning journey! 🚀
