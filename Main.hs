module Main where
  
import Data.Maybe(catMaybes)
import Data.List(intercalate)
import System.Environment(getProgName,getArgs)

import Words (allWords)
import Hint (readHints, matchesHints)

main :: IO()
main = do
  args <- getArgs  
  let hints = catMaybes $ map readHints args
  if null hints 
    then do
      progName <- getProgName
      putStrLn $ "Usage: " ++ progName ++ " pattern1 pattern2 ... patternN\n"
      putStrLn "  Shows all five letter words matching all given patterns.\n"
      putStrLn "  Example: pattern +a-li-ke shows all words that"
      putStrLn "    - contain an 'a' but not on the first place"
      putStrLn "    - do not contain an 'l' at any place"
      putStrLn "    - contain an 'i' at the third place"
      putStrLn "    - do not contain a 'k' at any place"
      putStrLn "    - contain an 'e' at the last place\n"
    else do
      let words = filter (`matchesHints` hints) allWords
      putStrLn $ intercalate "," words
      putStrLn $ show (length words) ++ " matches"
  

