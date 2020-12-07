module Main where

data Place = Place Int Int
  deriving (Show)

avgIndex :: Int -> Int -> Int
avgIndex min max = div (min + max) 2

findPlace :: (String, String) -> Place
findPlace (row, col) = Place (find row 0 127 'F' 'B') (find col 0 7 'L' 'R')
  where
    find (x : xs) min max l u | x == l = find xs min (avgIndex min max) l u
    find (x : xs) min max l u | x == u = find xs (avgIndex (min + 1) max) max l u
    find _ min _ _ _ = min

getSeatID :: Place -> Int
getSeatID (Place row col) = row * 8 + col

toSeatID :: String -> Int
toSeatID x = getSeatID (findPlace (splitAt 7 x))

main :: IO ()
main = do
  content <- readFile "input.txt"
  let contentLines = lines content
  let seatIDs = map toSeatID contentLines
  let partOne = maximum seatIDs
  let copy = [(minimum seatIDs) .. (maximum seatIDs)]
  let partTwo = head $ filter (`notElem` seatIDs) copy
  print partOne
