# 🎭 05 – Pattern Matching

In the previous topic on Lists & Tuples, you saw some examples like this:

```haskell
processStudent (name, age, grade) = name ++ " scored " ++ show grade
```

You might have wondered: "How does Haskell know that `name` is the first part of the tuple?" The answer is **pattern matching** - one of Haskell's most powerful and elegant features.

---

## 🔍 What Is Pattern Matching?

**Pattern matching** is like having X-ray vision for your data. Instead of using separate functions to extract pieces from data structures, you can **take them apart directly** in your function definitions.

Think of it this way: when you define a function, you're not just saying "this function takes some data." You're saying "this function takes data that **looks like this specific pattern**."

> **📺 Recommended Additional Resource:** [Pattern Matching in Haskell](https://youtu.be/RJjETqGwk5M?si=j2JIBzzTKFN9iEcC) by LigerLearn - well explained with examples.

Let's start with the simplest example:

```haskell
describeNumber :: Int -> String
describeNumber 0 = "This is zero"
describeNumber 1 = "This is one"
describeNumber _ = "This is some other number"
```

Here's what's happening:

-   The **first line** matches when the input is exactly `0`
-   The **second line** matches when the input is exactly `1`
-   The **third line** matches anything else (the `_` means "I don't care what this value is")

When you call `describeNumber 0`, Haskell tries each pattern **from top to bottom** until it finds one that matches. Since the first pattern matches `0`, that's the one that gets used.

The key insight: you're not just checking what the value is - you're **describing what pattern the input should match**.

---

## 🎯 The Wildcard Pattern: `_`

Before we go further, let's understand the **wildcard pattern** `_`. This is a special pattern that means "match anything, but I don't need to use the value."

```haskell
-- This function only cares if the input is 5, nothing else matters
isItFive :: Int -> String
isItFive 5 = "Yes, it's five!"
isItFive _ = "Nope, not five"
```

The `_` is perfect when you need to handle "everything else" but don't actually need to use the value in your code.

**Why not just use a variable name?** You could write:

```haskell
isItFive 5 = "Yes, it's five!"
isItFive x = "Nope, not five"  -- x would contain the value
```

But `_` makes it **crystal clear** that you don't care about the actual value - you're just providing a catch-all case.

---

## 📦 Pattern Matching with Tuples

Here's where pattern matching gets really powerful. Remember from the last topic how we extracted values from tuples using `fst` and `snd`?

```haskell
-- The old way (still works, but clunky)
addTupleOldWay :: (Int, Int) -> Int
addTupleOldWay pair = fst pair + snd pair
```

With pattern matching, we can **take the tuple apart right in the function definition**:

```haskell
-- The pattern matching way (elegant!)
addTuple :: (Int, Int) -> Int
addTuple (x, y) = x + y
```

What's happening here? The pattern `(x, y)` is telling Haskell: "This function expects a tuple with two elements. Call the first element `x` and the second element `y`, then I can use both names in my function body."

Pattern matching works for any size tuple:

```haskell
-- Extract a person's name from a record
getName :: (String, Int, Bool) -> String
getName (name, age, isStudent) = name
```

The pattern `(name, age, isStudent)` takes apart the tuple and gives names to each piece. We return just the `name`.

Since we don't use `age` or `isStudent`, wildcards are clearer:

```haskell
-- Clearer - shows we ignore the other values
getName :: (String, Int, Bool) -> String
getName (name, _, _) = name
```

The pattern `(name, _, _)` means: "This is a 3-tuple. Give me the first element as `name`, and I don't care about the other two."

---

## 📋 Pattern Matching with Lists: The Game Changer

This is where pattern matching becomes absolutely essential. Remember the "empty list trap" from the previous topic? Functions like `head` and `tail` crash when you give them an empty list.

Pattern matching **solves this completely** by letting you handle different list structures safely.

### The Empty List Pattern

The simplest list pattern is the empty list:

```haskell
-- Safe version of the length function
safeLength :: [a] -> Int
safeLength [] = 0  -- If the list is empty, length is 0
```

The pattern `[]` matches **only** the empty list. If your list has any elements at all, this pattern won't match.

### The Cons Pattern: `(x:xs)`

Now for the most important pattern in all of Haskell: the **cons pattern**.

Remember the `:` operator from the previous topic? It adds an element to the front of a list:

```haskell
1 : [2, 3]  -- Result: [1, 2, 3]
```

The cons **pattern** works **in reverse** - it takes a list apart. Where `:` **builds** a list, `(x:xs)` **breaks it down**:

```haskell
-- Building with :           Taking apart with (x:xs)
1 : [2, 3] = [1, 2, 3]  ←→  (x:xs) where x=1, xs=[2,3]
```

Here's how it works in practice:

```haskell
-- Safe version of head
safeHead :: [a] -> Maybe a
safeHead [] = Nothing           -- Empty list case
safeHead (x:xs) = Just x        -- Non-empty list case: extract the x part
```

The `Maybe` type represents "might not have a value" - `Nothing` for no value, `Just x` for a value.

When you call `safeHead [1,2,3]`, the pattern `(x:xs)` **reverses** the construction:

-   The list `[1,2,3]` was built like `1:[2,3]`
-   So the pattern matches with `x = 1` and `xs = [2,3]`

The beauty is that **the pattern tells you the structure**. If you see `(x:xs)`, you immediately know this function expects a non-empty list and will work with the first element and the rest separately.

### Why `xs` for the Tail?

You might wonder why we use `xs` (pronounced "x-s") for the tail. It's a Haskell convention:

-   `x` = a single element
-   `xs` = multiple elements (the plural of `x`)

You'll see this pattern everywhere in Haskell code. It's not required, but it makes code more readable.

### List Pattern Shortcuts

So far we've used the cons pattern `(x:xs)` for non-empty lists. But what if you want to match lists with a specific number of elements?

Haskell gives you shortcuts. Instead of writing the full cons pattern, you can write:

```haskell
-- These are shortcuts for common patterns
[x]      -- matches exactly one element (shortcut for x:[])
[x, y]   -- matches exactly two elements (shortcut for x:y:[])
[x, y, z] -- matches exactly three elements (shortcut for x:y:z:[])
```

Let's see this in action:

```haskell
describeList :: [a] -> String
describeList [] = "Empty list"
describeList [x] = "List with one element"          -- Only matches [42], ["hello"], etc.
describeList [x, y] = "List with exactly two elements" -- Only matches [1,2], ["a","b"], etc.
describeList (x:xs) = "List with many elements"        -- Matches [1,2,3], [1,2,3,4], etc.
```

**Key point:** `[x]` is just a convenient way to write `(x:[])`. Both mean "one element followed by empty list."

**Important:** Pattern order matters! Haskell tries patterns from top to bottom, so put **more specific patterns first**:

```haskell
-- ✅ Correct order
describeList [] = "Empty"        -- Most specific
describeList [x] = "One"         -- More specific
describeList (x:xs) = "Many"     -- General case

-- ❌ Wrong order
describeList (x:xs) = "Many"     -- This would match EVERYTHING non-empty
describeList [x] = "One"         -- This would NEVER be reached!
```

---

## 🔄 Recursion with Pattern Matching

Pattern matching and recursion work beautifully together. Every recursive function on lists follows the same pattern:

1. **Base case**: What to do with an empty list `[]`
2. **Recursive case**: What to do with a non-empty list `(x:xs)`

Let's build up to this with a simple example - calculating the length of a list:

```haskell
myLength :: [a] -> Int
myLength [] = 0                    -- Base case: empty list has length 0
myLength (x:xs) = 1 + myLength xs  -- Recursive: 1 + length of the rest
```

Let's trace through `myLength [1,2,3]`:

1. Pattern `(x:xs)` matches with `x=1`, `xs=[2,3]`
2. Returns `1 + myLength [2,3]`
3. For `[2,3]`: `x=2`, `xs=[3]`, returns `1 + myLength [3]`
4. For `[3]`: `x=3`, `xs=[]`, returns `1 + myLength []`
5. For `[]`: base case, returns `0`
6. Final result: `1 + 1 + 1 + 0 = 3`

The pattern is always the same:

-   **Empty list**: return some base value
-   **Non-empty list**: do something with the first element, then recurse on the rest

---

## 🛡️ Guards: When to Use Them with Patterns

Sometimes you need to check **conditions** as well as **structure**. That's where **guards** come in.

**Guards** are the `|` symbol followed by a condition. They let you add extra tests to your patterns:

```haskell
classifyNumber :: Int -> String
classifyNumber x
  | x > 0     = "Positive"
  | x < 0     = "Negative"
  | otherwise = "Zero"
```

The keyword **`otherwise`** is just another way to write `True` - it's the final catch-all guard.

### Combining Patterns and Guards

The real power comes from combining pattern matching (for structure) with guards (for conditions):

```haskell
classifyList :: [Int] -> String
classifyList [] = "Empty list"              -- Pattern: empty list
classifyList [x]                            -- Pattern: single element list
  | x > 0     = "Single positive number"    -- Guard: check if positive
  | otherwise = "Single non-positive number"
classifyList (x:xs)                         -- Pattern: multiple elements
  | length xs > 5 = "Long list"            -- Guard: check length
  | otherwise     = "Short list"
```

Here we're using **patterns** to identify the structure (empty, single element, multiple elements) and **guards** to test conditions on the values.

**When to use what:**

-   **Pattern matching**: For different data structures or specific values
-   **Guards**: For testing conditions or ranges
-   **Both together**: For maximum power and clarity

---

## 🎪 The `case` Expression

Sometimes you need to pattern match **in the middle of an expression**, not just in function definitions. That's where `case` expressions come in:

```haskell
-- Pattern matching in a function definition (what we've been doing)
describeLength :: [a] -> String
describeLength [] = "empty"
describeLength [x] = "one item"
describeLength xs = "multiple items"

-- Pattern matching in the middle of an expression
describeLengthCase :: [a] -> String
describeLengthCase xs = "This list is " ++
  case xs of
    [] -> "empty"
    [x] -> "one item"
    _ -> "multiple items"
```

The syntax is:

```haskell
case valueToMatch of
  pattern1 -> result1
  pattern2 -> result2
  _ -> defaultResult
```

`case` expressions are useful when you need to pattern match as part of a larger expression, rather than defining separate function cases.

---

## 🔍 Advanced Pattern: The `@` Symbol (As-Patterns)

Sometimes you want **both** the whole thing **and** its parts. The **as-pattern** (using `@`) lets you do this:

```haskell
processWholeAndParts :: [Int] -> String
processWholeAndParts [] = "Empty list"
processWholeAndParts xs@(x:rest) =
  "The whole list " ++ show xs ++
  " starts with " ++ show x ++
  " and has " ++ show (length rest) ++ " more items"
```

The pattern `xs@(x:rest)` means:

-   `xs` = the whole original list
-   `x` = the first element
-   `rest` = everything after the first element

So for `processWholeAndParts [1,2,3]`:

-   `xs = [1,2,3]` (the whole list)
-   `x = 1` (first element)
-   `rest = [2,3]` (the rest)

**Output:** `"The whole list [1,2,3] starts with 1 and has 2 more items"`

This gives you access to both the complete structure and its components.

---

## ⚠️ Common Mistakes and How to Avoid Them

### 1. Pattern Order Matters

```haskell
-- ❌ Wrong - the general pattern catches everything
badFunction :: [Int] -> String
badFunction (x:xs) = "Non-empty"  -- This matches ALL non-empty lists
badFunction [x] = "One element"   -- This will NEVER be reached!

-- ✅ Right - specific patterns first
goodFunction :: [Int] -> String
goodFunction [] = "Empty"
goodFunction [x] = "One element"     -- Specific case first
goodFunction (x:xs) = "Multiple"     -- General case last
```

### 2. Forgetting the Base Case

```haskell
-- ❌ Wrong - what happens with an empty list?
badLength :: [a] -> Int
badLength (x:xs) = 1 + badLength xs  -- Missing base case!

-- ✅ Right - always handle the base case
goodLength :: [a] -> Int
goodLength [] = 0                     -- Base case first
goodLength (x:xs) = 1 + goodLength xs
```

### 3. Using Partial Functions

```haskell
-- ❌ Dangerous - crashes on empty lists
unsafeHead :: [a] -> a
unsafeHead (x:xs) = x  -- What if the list is empty?

-- ✅ Safe - handles all cases
safeHead :: [a] -> Maybe a
safeHead [] = Nothing      -- Handle empty case
safeHead (x:xs) = Just x   -- Handle non-empty case
```

---

## 🎯 Putting It All Together: A Real Example

Let's build a function that safely gets the second element from a list, showing off everything we've learned:

```haskell
secondElement :: [a] -> Maybe a
secondElement [] = Nothing              -- Empty list: no second element
secondElement [_] = Nothing             -- One element: no second element
secondElement (_:y:_) = Just y          -- Two or more: second is y
```

Let's trace through the patterns:

-   `[]` matches only empty lists
-   `[_]` matches lists with exactly one element (we don't care what it is)
-   `(_:y:_)` matches lists with at least two elements:
    -   First `_` = we don't care about the first element
    -   `y` = the second element (this is what we want!)
    -   Last `_` = we don't care about the rest

This function is **total** (handles all possible inputs) and **safe** (never crashes).

---

## 🔗 What's Next?

You now understand:

-   ✅ How pattern matching **takes data apart safely**
-   ✅ The **wildcard pattern** `_` for values you don't need
-   ✅ **Tuple patterns** for extracting multiple values at once
-   ✅ The **cons pattern** `(x:xs)` for working with lists
-   ✅ How **recursion** and pattern matching work together
-   ✅ When to use **guards** vs **patterns**
-   ✅ **`case` expressions** for pattern matching in expressions
-   ✅ **As-patterns** `@` for accessing both whole and parts
-   ✅ How to write **safe, total functions**

**Now practice!** Complete the exercises in [`exercise.hs`](../exercise.hs) to master these concepts with hands-on coding.

**Next up**: **Recursion** - you've seen recursive patterns here, but we'll dive deep into the art and science of recursive thinking in Haskell!
