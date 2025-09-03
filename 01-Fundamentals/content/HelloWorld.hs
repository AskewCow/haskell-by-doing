-- HelloWorld.hs
-- Your first Haskell program!
-- Try running this file to see Haskell in action.

module Main where

main :: IO ()
main = putStrLn "Hello, Haskell!"

{- 
🚀 TO RUN THIS PROGRAM:

⚠️  IMPORTANT: Make sure your terminal is in the RIGHT FOLDER! 📁

🎯 Method 1 (Recommended - VS Code):
   1️⃣  Right-click on this file (HelloWorld.hs) in the VS Code file explorer 📂
   2️⃣  Select "Open in Integrated Terminal" 💻
   3️⃣  The terminal will open in the correct folder automatically ✅

🔧 Method 2 (Manual navigation):
   1️⃣  Open terminal and navigate to: cd "your-path/haskell-by-doing/01-Fundamentals/content" 🗂️
   2️⃣  You should see HelloWorld.hs when you run: dir (Windows) or ls (Mac/Linux) 👀

🏃‍♂️ Then run:
   📦 Compile & Run: ghc HelloWorld.hs -o hello
   ▶️  Execute: ./hello (or hello.exe on Windows)

🎨 Or use the interpreter (no compilation needed):
   🏃 Quick Run: runghc HelloWorld.hs

💡 Note: If your antivirus blocks the .exe file, just use the interpreter method (runghc) instead! 🛡️
-}

