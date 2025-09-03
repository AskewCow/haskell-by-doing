# 🐏 01 – Fundamentals

> 💡 **Tip:** Right-click this file and choose **"Open Preview"** (or press `Ctrl+Shift+V` / `Cmd+Shift+V`) for a better reading experience!

Welcome to Haskell Fundamentals! 🎉
This is your first step into the world of purely functional programming.

---

## 🔍 What is Haskell?

Haskell is a **pure functional programming language**. That means:

-   ✅ Functions always give the same output for the same input (no hidden surprises).
-   ✅ No changing variables- instead we create _new values_ (similar to a `final` variable in Java).
-   ✅ Strong typing helps catch errors early.
-   ✅ It's great for writing clean, reliable, and maintainable code.

Think of Haskell as uniform programming with **math-like precision** ✨.

---

### 🧩 Strong Typing- A deeper dive

In Haskell, **every value has a type**, and the compiler checks them strictly:

```haskell
x :: Int
x = 5

y :: String
y = "hello"

z = x + y   -- ❌ Error! You can't add Int and String
```

This helps you catch mistakes at compile time, rather than crashing at runtime.

---

### 🔒 Immutability (No Changing Variables)

In Java, if you write:

```java
final int x = 5;
x = 6;  // ❌ Error: cannot assign a value to final variable
```

Haskell takes this idea further: **all values are immutable by default**.

```haskell
x = 5
x = 6   -- ❌ Error: redefinition not allowed
```

Instead of changing `x`, you create new values:

```haskell
x = 5
y = x + 1   -- ✅ y is a new value, x stays the same
```

This makes Haskell programs **predictable and bug-resistant**.

---

## 📂 Structure of a `.hs` File

A minimal Haskell file looks like this:

```haskell
-- MyFirstProgram.hs
module Main where

main :: IO ()
main = putStrLn "Hello, Haskell!"
```

-   `module Main where` → declares the module (like a namespace).
-   `main :: IO ()` → the type of our entry point.
-   `putStrLn` → prints text followed by a newline.
-   `-- MyFirstProgram.hs` → is how you write a comment in Haskell (anything after `--` is ignored).
    -   For multi-line comments, use `{- comment content -}`.

---

## 🧾 `main :: IO ()` Explained

This line is **important**:

```haskell
main :: IO ()
```

-   `main` → the entry point of your program.
-   `IO ()` → says it performs an _I/O action_ (printing, reading, etc.) and returns nothing useful (`()`).
-   In Haskell, any real-world side effect (like printing to the screen) must live in the **IO world**.

<br/>

💡 **Why does Haskell wrap I/O in a special type?**
Haskell is a purely functional language. That means it avoids side effects by default.
So instead of letting effects happen anywhere, it isolates them inside `IO`.
This keeps your core logic pure and predictable, while still letting you print stuff when needed.

> 🔍 **Note:** There are many other types in Haskell beyond `IO ()`, but we'll explore those in future lessons!

## 🛠️ Essential I/O Functions for This Lesson

To complete the exercises, you’ll need these two building blocks:

### 📤 Printing to the screen

```haskell
putStrLn :: String -> IO ()
putStrLn "Hello, Haskell!"
```

-   Takes a `String`
-   Returns an `IO ()` action that prints it

### 📥 Reading input

```haskell
getLine :: IO String
```

-   Waits for the user to type something
-   Produces that input as a `String`

### 👉 Example program:

```haskell
main :: IO ()
main = do
    putStrLn "What is your name?"
    name <- getLine
    putStrLn ("Hello, " ++ name ++ "!")
```

**Notes:**

-   `do` lets you sequence multiple IO actions.
-   `<-` extracts the result of an IO action into a variable (`name` here).
-   `++` is the string concatenation operator.

---

## 👋 Hello World Program

```haskell
module Main where

main :: IO ()
main = putStrLn "Hello, Haskell!"
```

> 💡 **Try running the program yourself!** There's a ready-to-run file: [`HelloWorld.hs`](HelloWorld.hs) in this same folder. It's crucial that you do this short exercise to avoid issues later on!

---

## 🔗 What's Next?

Once you understand these fundamentals, you're ready to dive into **exercises** and start coding!

Remember: Haskell might feel different from other languages at first, but its precision and elegance will grow on you. 🌱
