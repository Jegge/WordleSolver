module Hint 
( Hints
, readHints
, matchesHints
) where
  
import Data.Char (isAlpha, toLower)
  
data Hint = Nowhere Char
          | NotHere Char
          | Correct Char
          deriving (Show)          
          
type Hints = [Hint]

readHints :: String -> Maybe Hints
readHints = sequence . readHint
  where
    readHint :: String -> [Maybe Hint]
    readHint ('-':c:cs) = (Nowhere <$> readChar c):readHint cs
    readHint ('+':c:cs) = (NotHere <$> readChar c):readHint cs
    readHint (c:cs)     = (Correct <$> readChar c):readHint cs
    readHint []         = []
    readChar :: Char -> Maybe Char  
    readChar c = if isAlpha c then Just (toLower c) else Nothing
      
matchesHints :: String -> [Hints] -> Bool
matchesHints s hss = all (\hs -> all (matchesHint s) $ zip s hs) hss
  where 
    matchesHint :: String -> (Char, Hint) -> Bool 
    matchesHint cs (a, (Correct b)) = a == b
    matchesHint cs (a, (NotHere b)) = a /= b && b `elem` cs
    matchesHint cs (_, (Nowhere b)) = not $ b `elem` cs