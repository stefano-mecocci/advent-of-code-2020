module Main where

-- part one

data Matrix = Matrix
  { game :: [[Char]],
    rows :: Int,
    cols :: Int
  }
  deriving (Show)

foo :: [[Char]] -> Int -> Int -> Int -> [Bool]
foo [] _ _ _ = []
foo (x : xs) step acc cols
  | pos <= cols = isTree pos x : foo xs step pos cols
  | pos > cols = isTree (pos - cols) x : foo xs step (pos - cols) cols
  where
    pos = acc + step

isTree :: Int -> [Char] -> Bool
isTree _ [] = False
isTree n (x : xs)
  | n > 1 = isTree (n - 1) xs
  | n == 1 = x == '#'

partOne x = foo (tail x) 3 1 (length $ head x)

main :: IO ()
main = do
  content <- readFile "input.txt"
  let contentLines = lines content
  print (length (filter id (partOne contentLines)))
