# 🐏 Learn Haskell - A GitHub-Driven Learning Course

Welcome to the **Haskell by Doing** course! This is a modern, GitHub-optimized path to mastering Haskell, designed to grow your skills **and** your profile ⭐.

Whether you're just starting out or brushing up, this course will walk you through functional programming concepts using a structured, visual, and hands-on approach.

---

## ✨ Why This Course?

-   ✅ Beginner-friendly explanations
-   🔍 Real-world code examples
-   🔁 Step-by-step challenges
-   🌱 Build incrementally- one concept at a time
-   🌐 Grow your GitHub with visible progress

---

<br/>

# 🧠 How This Course Works

## 1. 📖 Learn a Concept

-   Short, clear explanations
-   💡 Minimal but focused code examples
-   🎨 Visuals and edge cases explained

## 2. 🏋️‍♂️ Try It Yourself

-   Solve challenges in each lesson folder
-   ⬆️ Push your work to GitHub
-   📝 Add notes on what confused you

## 3. 🧩 Level Up Over Time

-   Each module builds on the last
-   🧠 Develop intuition, not just syntax

## 4. 🗒️ Document What You Learn

-   Use the built-in note system (or create your own)
-   🚩 Keep track of mistakes, gotchas, and "aha" moments

<br/>

---

<br/>

# 📝 Reading Notes and Lessons in VS Code

All lessons and notes use **Markdown (`.md`) files** for a clean, readable experience in VS Code.

> 💡 **Tip:**
>
> -   Right-click any `.md` file (like `README.md`, `01-Fundamentals.md`, etc.) and choose **“Open Preview”**
> -   Or use the shortcut:
>     -   **Mac:** `Cmd + Shift + V`
>     -   **Windows/Linux:** `Ctrl + Shift + V`

This opens a styled, easy-to-read view, perfect for:

| 📚 What?              | 📝 File(s)      |
| --------------------- | --------------- |
| Lecture explanations  | `/content/*.md` |
| Your personal notes   | `/notes/*.md`   |
| Exercise walkthroughs | lesson folders  |

You can also **split the editor** and open the preview side-by-side with the text for even better workflow!

<br/>

---

<br/>

# ⚙️ Getting Set Up

### 📦 Step 1: Install GHC, Cabal & Stack

**Install via [GHCup](https://www.haskell.org/ghcup/):**

```bash
curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh
```

This will install:

-   `ghc` → The Haskell compiler
-   `cabal` → Standard Haskell build tool
-   `stack` → Optional alternative build tool (you may not need it, but it's good to have)

After installation, **restart your terminal**, then check your install:

```bash
ghc --version
cabal --version
stack --version  # optional
```

✅ If you see version numbers, your installation is successful.

---

### 💻 Step 2: Install VS Code + Extensions

Download [Visual Studio Code](https://code.visualstudio.com/) if you don’t have it.

**Recommended Extensions:**

| Extension                      | Publisher   | Why                                                                 |
| ------------------------------ | ----------- | ------------------------------------------------------------------- |
| 🟣 Haskell                     | Haskell     | Full support: errors, warnings, type hints, go-to-definition (HLS). |
| 🎨 Haskell Syntax Highlighting | Justus Adam | Adds proper syntax coloring for `.hs` files.                        |

#### ⚠️ What you don't need:

-   Live Server or Code Runner → Not needed for Haskell
-   Old/unmaintained Haskell extensions → stick to the two above

<br/>

After installing the Haskell extension, VS Code will prompt you to install **Haskell Language Server (HLS)** if it isn’t already set up.  
👉 Accept that prompt.

With both extensions installed, you’ll get syntax highlighting + smart Haskell editing.

---

### ✅ Step 3: Clone the Course Repo

The course is pre-structured, i.e. you don’t need to create a new project. Just clone and open it:

```bash
git clone https://github.com/AskewCow/haskell-by-doing
cd haskell-course
code . # Opens folder in vsc
```

You’re ready to go! 🎉 Start with `01-Fundamentals/content`

---

### 🧪 Optional: Test That VS Code Works

Open any `.hs` file in VS Code and make sure you see:

-   Autocompletion
-   Type info when hovering over expressions
-   Inline error messages (if something’s wrong)
-   Haskell-specific color syntax

If all of those are working, then HLS is active and everything is set up properly.

<br/>

---

# 🚀 Ready to Start Your Haskell Journey?

<div align="center">
    
**You're all set!** 🎉 Dive into the lessons, experiment, and don't forget to jot down your discoveries and mistakes in your notes.

<img src="https://img.shields.io/badge/⭐-Star%20this%20repo%20if%20it%20helped%20you!-FFD700?style=for-the-badge&logo=github&logoColor=white" alt="Star this repo"/>

<br/>

<a href="https://github.com/AskewCow/haskell-by-doing/stargazers">
    <img src="https://img.shields.io/github/stars/AskewCow/haskell-by-doing?style=social&label=Star" alt="GitHub stars"/>
</a>

<br/>

**Happy Haskell-ing!** 🐏✨

</div>
