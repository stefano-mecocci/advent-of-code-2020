module Main where

type Slope = (Int, Int)

isTree :: Int -> [Char] -> Bool
isTree _ [] = False
isTree n (x : xs)
  | n > 1 = isTree (n - 1) xs
  | n == 1 = x == '#'

validPoints :: [[Char]] -> Slope -> Int -> Int -> [Bool]
validPoints [] _ _ _ = []
validPoints xs (right, down) acc cols
  | pos <= cols = isTree pos (head xs) : validPoints (drop down xs) (right, down) pos cols
  | pos > cols = isTree (pos - cols) (head xs) : validPoints (drop down xs) (right, down) (pos - cols) cols
  where
    pos = acc + right

trees :: [[Char]] -> Slope -> Int
trees x (right, down) = length (filter id (validPoints (drop down x) (right, down) 1 (length $ head x)))

partOne :: [[Char]] -> Int
partOne x = trees x (3, 1)

partTwo :: [[Char]] -> Int
partTwo x = product $ map (trees x) [(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)]

main :: IO ()
main = do
  content <- readFile "test.txt"
  let contentLines = lines content
  print $ partOne contentLines
