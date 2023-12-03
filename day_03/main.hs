import System.IO
import Data.Char (isDigit)
import Data.List (elemIndices)

processFile :: FilePath -> IO Int
-- processFile :: FilePath -> IO [Int]
processFile filePath = do
  handle <- openFile filePath ReadMode
  result <- processFileContent handle [] 0
  hClose handle
  return result

processFileContent :: Handle -> [String] -> Int -> IO Int
processFileContent handle lines carry
  -- INIT (List has incorrect length.)
  | length lines /= 3 = do
    line_1 <- hGetLine handle
    line_2 <- hGetLine handle
    processFileContent handle ["", line_1, line_2] carry
  -- FINISH (Current line is not set.)
  | lines !! 1 == "" = return carry
  -- PROCESS
  | otherwise = do
    -- let result = carry + sum (processLinesPart1 lines)
    let result = carry + processLinesPart2 lines
    hasReachedEndOfFile <- hIsEOF handle
    if hasReachedEndOfFile then do
      processFileContent handle [lines !! 1, lines !! 2, ""] result
    else do
      newLine <- hGetLine handle
      let newLines = [lines !! 1, lines !! 2, newLine]
      processFileContent handle newLines result

processLinesPart1 :: [String] -> [Int]
processLinesPart1 lines =
  map (\(start, end) ->                                           -- 4.0: Parse the numbers from the "line" for the remaining indexes.  
    read $                                                        --  .2: Parse the string into an Int.
    substring (start, end) (lines !! 1)                           --  .1: Get the string on the position of the current line.
  ) .
  filter (\(start, end) ->                                        -- 3.0 - Filter the ones that do not have an adjecent special character.
    any (any (`notElem` "1234567890.") .                          --  .2: Confirm if there's any non-blacklisted character.
    substring (start - 1, end + 1)) lines                         --  .1: Get the string on the position and the surrounding items.
  ) .
  convertToRanges $                                               -- 2.0 - Convert them to ranges: [(0,0), (2,3), (5,7)]
  [index | (char, index) <- zip (lines !! 1) [0..], isDigit char] -- 1.0 - Find indexes of numeric digits: [0, 2, 3, 5, 6, 7]

processLinesPart2 :: [String] -> Int
processLinesPart2 lines = do
  let numberRanges =
        map (\line ->
          convertToRanges [index | (char, index) <- zip line [0..], isDigit char]
        ) lines
  let gearNumbers =
        filter (\numbers -> length numbers == 2) $
        map (\position ->
          concat $
          zipWith (\ranges index ->
            map (\(start, end) -> read (substring (start, end) (lines !! index)) :: Int) .
            filter (\(start, end) -> start <= position + 1 && end >= position - 1) $
            ranges
          ) numberRanges [0..]
        ) (elemIndices '*' $ lines !! 1)
  sum (map product gearNumbers)

convertToRanges :: [Int] -> [(Int, Int)]
convertToRanges [] = []
convertToRanges (x:xs) = go x x xs
  where
    go start prev (y:ys)
      | y == prev + 1 = go start y ys
      | otherwise = (start, prev) : go y y ys
    go start prev [] = [(start, prev)]

substring :: (Int, Int) -> String -> String
substring (start, end) str = drop start (take (end + 1) str)

main :: IO ()
main = do
  result <- processFile "input.txt"
  putStr "Part 1: "
  putStr (show result)
  putStr "\n"
