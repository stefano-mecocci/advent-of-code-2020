module Main where

import Data.List.Split (splitOn)

-- part one

getBounds :: [String] -> [Int]
getBounds (x : _) = map (read :: String -> Int) (splitOn "-" x)

getCh :: [String] -> Char
getCh (_ : x : _) = head x

count :: Char -> String -> Int
count _ [] = 0
count c (x : xs) | c == x = 1 + count c xs
count c (_ : xs) = count c xs

partOne :: String -> Bool
partOne el = occurences >= min && occurences <= max
  where
    occurences = count (getCh (words el)) (last (words el))
    range = getBounds (words el)
    min = head range
    max = last range

-- part two

xor :: Bool -> Bool -> Bool
xor a b = a /= b

elemAt :: Int -> [Char] -> Char
elemAt _ [] = ' ' -- a password cannot contain a space
elemAt n (x : xs)
  | n == 1 = x
  | otherwise = elemAt (n - 1) xs

partTwo :: String -> Bool
partTwo el = (minchar == getCh (words el)) `xor` (maxchar == getCh (words el))
  where
    range = getBounds (words el)
    min = head range
    max = last range
    minchar = elemAt min (last (words el))
    maxchar = elemAt max (last (words el))

main :: IO ()
main = do
  content <- readFile "input.txt"
  let contentLines = lines content
  let validPasswords = map partTwo contentLines
  print (foldl (\acc x -> if x then acc + 1 else acc) 0 validPasswords)
