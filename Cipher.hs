module Cipher where

import Char

-- Remove non-alphabetic chars and make uppercase.
normalize :: String -> String
normalize = filter isUpper . map toUpper

-- Ceasar cipher.
caesar :: Int -> String -> String
caesar offset = map (shift offset) . normalize

-- Offset for the char in the key at given index.
offsetForIndex :: String -> Int -> Int
offsetForIndex key idx = index $ key !! i
  where i = idx `mod` (length key)

-- Vigenere cipher.
vigenere :: String -> String -> String
vigenere key plain = zipWith (\ c i -> shift (offsetForIndex normKey i) c) normPlain [0..]
  where normKey = normalize key
        normPlain = normalize plain

main :: IO ()
main = do
  putStrLn "Input words to encrypt:"
  words <- getLine

  putStrLn "Input offset for Ceasar cipher:"
  offsetStr <- getLine
  let offset = read offsetStr :: Int
  putStrLn $ caesar offset words

  putStrLn "Input key for Vigenere cipher:"
  key <- getLine
  putStrLn $ vigenere key words

