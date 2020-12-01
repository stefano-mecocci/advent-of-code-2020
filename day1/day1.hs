module Main where

-- part 1
twoElems :: [Int] -> [[Int]]
twoElems xs = [[x, y] | x <- xs, y <- xs, x + y == 2020]

allDifferent :: (Eq a) => [a] -> Bool
allDifferent []     = True
allDifferent (x:xs) = x `notElem` xs && allDifferent xs

-- part 2
threeElems :: [Int] -> [[Int]]
threeElems xs = [[x, y, z] | x <- xs, y <- xs, z <- xs, x + y + z == 2020]

main = do
  content <- readFile "input.txt"
  let contentLines = lines content
  let numbers = map (read::String->Int) contentLines
  print (product (head (filter allDifferent (twoElems numbers))))
