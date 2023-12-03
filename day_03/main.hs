import Data.Char
import Data.List
import System.Environment
import System.IO

ignoredAdjecents = "1234567890."
validFlags       = ["-v1", "-v2"]

--- App Init ---

main :: IO ()
main = do
  args <- getArgs
  case processArguments args of
    Left errorMessage -> putStrLn $ "Error: " ++ errorMessage
    Right (inputPath, processFunc) -> do
      result <- processFile inputPath processFunc
      putStrLn $ "Result: " ++ show result

processArguments :: [String] -> Either String (String, [String] -> Int)
processArguments args
  | null inputPath               = Left "Must specify one input file"
  | not (null invalidInputPaths) = Left "Multiple input paths specified"
  | not (null invalidFlags)      = Left "Invalid flags provided"
  | otherwise                    = Right (inputPath, processFunc)
  where
    inputPaths        = filter (\arg -> head arg /= '-') args
    invalidInputPaths = drop 1 inputPaths
    [inputPath]       = inputPaths
    flagArgs          = filter (\arg -> head arg == '-') args
    invalidFlags      = filter (`notElem` validFlags) flagArgs
    processFunc
      | "-v1" `elem` flagArgs = processFuncPart1
      | "-v2" `elem` flagArgs = processFuncPart2
      | otherwise             = processFuncPart2

--- Handling file reading, and the processing of it in shifting chunks of 3 lines ---

processFile :: FilePath -> ([String] -> Int) -> IO Int
processFile filePath processFunc =
  withFile filePath ReadMode $ \handle ->
    processFileContent handle processFunc [] 0

processFileContent :: Handle -> ([String] -> Int) -> [String] -> Int -> IO Int
processFileContent handle processFunc lines carry
  | length lines /= 3 = do
    line_1 <- hGetLine handle
    line_2 <- hGetLine handle
    processFileContent handle processFunc ["", line_1, line_2] carry
  | currentLine == "" = return carry
  | otherwise = do
      hasReachedEndOfFile <- hIsEOF handle
      let getNextLines = if hasReachedEndOfFile
          then return [currentLine, nextLine, ""]
          else do
            newLine <- hGetLine handle
            return [currentLine, nextLine, newLine]
      newLines <- getNextLines
      processFileContent handle processFunc newLines result
  where
    result = carry + processFunc lines
    [_, currentLine, nextLine] = lines

--- Part 1 Implementation ---

processFuncPart1 :: [String] -> Int
processFuncPart1 lines =
  sum $
    map (\(start, end) -> read (substring (start, end) currentLine)) .
    filter (\(start, end) ->
      any (any (`notElem` ignoredAdjecents) .
      substring (start - 1, end + 1)) lines
    ) $
    numberRanges
  where
    currentLine         = lines !! 1
    numberRanges        = getRangesOfConsecutiveNumbers $ getDigitIndices currentLine
    getDigitIndices str = [index | (char, index) <- zip str [0..], isDigit char]

--- Part 2 Implementation ---

processFuncPart2 :: [String] -> Int
processFuncPart2 lines = sum (map product gearNumbers)
  where
    currentLine = lines !! 1
    numberRanges =
      map (\line ->
        getRangesOfConsecutiveNumbers $
        [index | (char, index) <- zip line [0..], isDigit char]
      ) lines
    gearNumbers =
      filter (\numbers -> length numbers == 2) $
      map (\position ->
        concat $
        zipWith (\ranges index ->
          map (\(start, end) -> read (substring (start, end) (lines !! index)) :: Int) .
          filter (\(start, end) -> start <= position + 1 && end >= position - 1) $
          ranges
        ) numberRanges [0..]
      ) (elemIndices '*' currentLine)

--- Utility Functions ---

getRangesOfConsecutiveNumbers :: [Int] -> [(Int, Int)]
getRangesOfConsecutiveNumbers [] = []
getRangesOfConsecutiveNumbers (x:xs) = go x x xs
  where
    go start prev (y:ys)
      | y == prev + 1 = go start y ys
      | otherwise = (start, prev) : go y y ys
    go start prev [] = [(start, prev)]

substring :: (Int, Int) -> String -> String
substring (start, end) = take (end - start + 1) . drop start
