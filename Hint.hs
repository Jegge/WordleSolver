module Hint 
( Hints
, readHints
, matchHints
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
    readHint ('-':c:cs) = (Nowhere <$> readChar c):(readHint cs)
    readHint ('+':c:cs) = (NotHere <$> readChar c):(readHint cs)
    readHint (c:cs)     = (Correct <$> readChar c):(readHint cs)
    readHint []         = []
    readChar :: Char -> Maybe Char  
    readChar c = if isAlpha c then Just (toLower c) else Nothing
      
matchHints :: [Hints] -> String -> Bool
matchHints hss s = all (\hs -> all (matchHint s) $ zip s hs) hss
  where 
    matchHint :: String -> (Char, Hint) -> Bool 
    matchHint cs (a, (Correct b)) = a == b
    matchHint cs (a, (NotHere b)) = a /= b && b `elem` cs
    matchHint cs (_, (Nowhere b)) = not $ b `elem` cs

