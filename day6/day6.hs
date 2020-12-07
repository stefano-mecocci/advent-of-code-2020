module Main where

import Data.List (intersect, nub)
import Data.List.Split (splitOn)

groupToYesUn :: String -> [Char]
groupToYesUn x = nub $ concat (lines x)

groupToYesInt :: String -> [Char]
groupToYesInt x = foldl intersect ['a' .. 'z'] (lines x)

main :: IO ()
main = do
  content <- readFile "input.txt"
  let contentLines = splitOn "\n\n" content
  let partOne = sum $ map (length . groupToYesUn) contentLines
  let partTwo = sum $ map (length . groupToYesInt) contentLines
  print partTwo
