module Main where

import Cipher

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

