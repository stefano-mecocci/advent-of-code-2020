module Main where

import Data.Char (isDigit, isHexDigit)
import Data.List (isSuffixOf)
import Data.List.Split (splitOn)
import Data.Map (Map)
import qualified Data.Map as Ma
import Data.Maybe (fromJust)

splitPassports :: [String] -> [[String]]
splitPassports = map words

listToTuple :: [a] -> (a, a)
listToTuple xs = (head xs, last xs)

toTuple :: String -> (String, String)
toTuple prop = listToTuple (splitOn ":" prop)

fromString :: [[String]] -> [[(String, String)]]
fromString = map (map toTuple)

requiredMembers :: [String]
requiredMembers = ["byr", "iyr", "eyr", "hgt", "hcl", "pid", "ecl"]

tuplesToMaps :: [[(String, String)]] -> [Map String String]
tuplesToMaps = map Ma.fromList

requiredFields :: Map String String -> Bool
requiredFields x = all (`Ma.member` x) requiredMembers

partOne :: [Map String String] -> Int
partOne x = sum (map (fromEnum . requiredFields) x)

-- part two

inRange :: String -> Int -> Int -> Bool
inRange s min max = n >= min && n <= max
  where
    n = (read :: String -> Int) s

validBirth :: Map String String -> Bool
validBirth p = length birth == 4 && inRange birth 1920 2002
  where
    birth = fromJust (Ma.lookup "byr" p)

validIssue :: Map String String -> Bool
validIssue p = length iyr == 4 && inRange iyr 2010 2020
  where
    iyr = fromJust (Ma.lookup "iyr" p)

validExpiration :: Map String String -> Bool
validExpiration p = length eyr == 4 && inRange eyr 2020 2030
  where
    eyr = fromJust (Ma.lookup "eyr" p)

validPid :: Map String String -> Bool
validPid p = length pid == 9 && all isDigit pid
  where
    pid = fromJust (Ma.lookup "pid" p)

validEyeColor :: Map String String -> Bool
validEyeColor passport = ecl `elem` ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]
  where
    ecl = fromJust (Ma.lookup "ecl" passport)

validHairColor :: Map String String -> Bool
validHairColor passport = head hcl == '#' && all isHexDigit (tail hcl)
  where
    hcl = fromJust (Ma.lookup "hcl" passport)

validHeight :: Map String String -> Bool
validHeight passport
  | "cm" `isSuffixOf` hgt = inRange hgtNum 150 193
  | "in" `isSuffixOf` hgt = inRange hgtNum 59 76
  where
    hgt = fromJust (Ma.lookup "hgt" passport)
    hgtNum = init (init hgt)
validHeight _ = False

criteria :: Map String String -> Bool
criteria p = validBirth p && validIssue p && validPid p && validExpiration p && validHairColor p && validEyeColor p && validHeight p

requiredAndCriteria :: Map String String -> Bool
requiredAndCriteria p = requiredFields p && criteria p

partTwo :: [Map String String] -> Int
partTwo passports = sum (map (fromEnum . requiredAndCriteria) passports)

main :: IO ()
main = do
  content <- readFile "input.txt"
  let passportsStrings = splitPassports (splitOn "\n\n" content)
  let passports = tuplesToMaps (fromString passportsStrings)
  print $ partTwo passports
