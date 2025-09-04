# 🐏 02 – Expressions & Types

Welcome to **Expressions & Types**! 🌱
This is where Haskell really shows its **math-like nature**: _everything is an expression_ and _every expression has a type_.

---

## 🧮 Everything is an Expression

In many languages you have a **mix** of _statements_ and _expressions_:

-   **Statements** do something, but don't return a value (`if (...) { ... }` in Java).
-   **Expressions** always produce a value (`1 + 2`).

In Haskell, there are **only expressions**. That means:

-   ✅ **Uniformity** - you don't have to remember which constructs return values and which don't.
-   ✅ **Composability** - since everything gives back a value, you can nest expressions freely.
-   ✅ **Predictability** - it's easier to reason about code, since each part is like a little math formula.

Example:

```java
// Java
if (x > 0) {
    y = 1;
} else {
    y = -1;
}
```

```haskell
-- Haskell (expression directly produces a value)
y = if x > 0 then 1 else -1
```

Even `do` blocks in I/O are expressions under the hood.
This consistency makes reasoning about Haskell programs much easier.

---

## 🔢 Expressions Produce Types

If everything is an expression, the natural next question is:
_"What kind of value does this expression produce?"_ → That's where **types** come in.

Every expression has a type:

-   `3` → `Int`
-   `True` → `Bool`
-   `"Hello"` → `String`
-   `if x > 0 then 1 else -1` → `Int`

---

## 🔢 Basic Types

Here are the **essential building blocks**:

| Type      | Example               | Notes                                                    |
| --------- | --------------------- | -------------------------------------------------------- |
| `Int`     | `42`                  | Fixed-size integer (fast, but can overflow)              |
| `Integer` | `1234567890123456789` | Arbitrary precision integer (slower, but unlimited size) |
| `Float`   | `3.14`                | Single-precision floating point                          |
| `Double`  | `2.71828`             | Double-precision floating point                          |
| `Bool`    | `True`, `False`       | Boolean values                                           |
| `Char`    | `'a'`, `'Z'`          | A single Unicode character (use `'` quotes)              |
| `String`  | `"Hello"`             | A list of `Char` (`[Char]`)                              |

---

### ✨ Examples

```haskell
-- Integers
x :: Int
x = 10

big :: Integer
big = 123456789012345678901234567890

-- Floating points
piApprox :: Float
piApprox = 3.14

e :: Double
e = 2.718281828

-- Boolean
flag :: Bool
flag = True

-- Characters and strings
letter :: Char
letter = 'A'

greeting :: String
greeting = "Hello!"
```

---

## 🧠 Type Inference

One of Haskell's **superpowers**:
You don't always need to write types- the compiler figures them out!

```haskell
x = 5           -- Inferred as Int (default for numbers)
name = "Alice"  -- Inferred as String
```

But you _can_ add type annotations for clarity (and you should, as a good habit):

```haskell
x :: Int
x = 5
```

📝 Note: **Type inference** makes your code concise, while **type annotations** make it more readable and self-documenting.

---

## 🖥️ Exploring Types in GHCi

Running these commands inside GHCi:

```haskell
-- Check the type of an expression
:t 42
-- Int

:t 42 :: Integer
-- Integer

:t 'a'
-- Char

:t "hello"
-- [Char]  (a String is just a list of Char)

:t True
-- Bool

:t not True
-- Bool
```

> 🔍 In the upcoming exercise you will see that GHCi is like a **calculator on steroids** for types and values.

---

## 🌀 A Glimpse of Polymorphism

Some functions in Haskell work with **many types**, not just one.

```haskell
:t length
-- length :: [a] -> Int
```

The `a` is a _type variable_ - meaning `length` works on a list of any type (`[Int]`, `[Char]`, `[Bool]`, …).
This is called **parametric polymorphism**, and it's one of Haskell's strongest features.

> 💡 **Don't worry if this doesn't make complete sense yet!** We'll dive much deeper into polymorphism, type variables, and how they work in later topics. For now, just know that some functions can work with many different types.

---

## ⚠️ Common Pitfalls

Here are some tricky spots that catch beginners (and even experienced developers!):

### 🔤 Char vs String Confusion

```haskell
'a'   -- Char (single character, single quotes)
"a"   -- String (list of characters, double quotes)
"ab"  -- String
'ab'  -- ERROR! Char can only hold one character
```

### 🔢 Number Type Defaults

```haskell
x = 42        -- Defaults to Int
y = 42.0      -- Defaults to Double (not Float!)
z = 42 :: Integer  -- Explicitly Integer for big numbers
```

💡 **Tip**: Use `Integer` for really big numbers, `Int` for everyday counting.

### 📖 The `read` Function Trap

```haskell
read "5"           -- ERROR! Haskell doesn't know what type you want
read "5" :: Int    -- ✅ Works! Now it knows to parse as Int
read "5" :: Double -- ✅ Same string, different type
read "hello" :: Int -- 💥 Runtime error! Can't parse "hello" as number
```

### ➕ Operator Type Mismatches

```haskell
5 + 2.5        -- ERROR! Can't mix Int and Double
5.0 + 2.5      -- ✅ Works! Both are Double
fromIntegral 5 + 2.5  -- ✅ Convert Int to Double first
```

### 📝 String Concatenation Confusion

```haskell
"Hello" + " World"     -- ERROR! No + for strings
"Hello" ++ " World"    -- ✅ Use ++ for string concatenation
show 42 ++ " items"    -- ✅ Convert number to string first
```

### 🔄 Division Surprises

```haskell
5 / 2          -- 2.5 (floating point division)
5 `div` 2      -- 2 (integer division, truncated)
5 `mod` 2      -- 1 (remainder/modulo)
```

---

## 🔗 What's Next?

Now that you understand **expressions and types**, it's time to practice! 💪

👉 **Head over to [`exercise.hs`](../exercise.hs)** and complete the exercises to solidify your understanding of types and expressions.

Once you've finished the exercises, you'll be ready to move on to **functions**, the real stars of Haskell! ⭐
