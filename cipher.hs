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

