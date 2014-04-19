module Char where

-- Returns index of uppercase char starting from A as 0.
index :: Char -> Int
index c = fromEnum c - fromEnum 'A'

-- Number of alphabets.
atoz :: Int
atoz = fromEnum 'Z' - fromEnum 'A' + 1

-- Shift char by offset.
shift :: Int -> Char -> Char
shift offset c = toEnum $ fromEnum 'A' + fromA
  where fromA = (index c + offset) `mod` atoz

-- Check if a char is lowercase.
isLower :: Char -> Bool
isLower c = 'a' <= c && c <= 'z'

-- Check if a char is uppercase.
isUpper :: Char -> Bool
isUpper c = 'A' <= c && c <= 'Z'

-- Lowercase char.
toLower :: Char -> Char
toLower c
  | isUpper c = toEnum $ fromEnum c + 32
  | otherwise = c

-- Uppercase char.
toUpper :: Char -> Char
toUpper c
  | isLower c = toEnum $ fromEnum c - 32
  | otherwise = c

