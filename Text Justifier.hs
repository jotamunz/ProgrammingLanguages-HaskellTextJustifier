import Data.List as List
import System.IO

texto = "Aquel que controla el pasado controla el futuro. Aquel que controla el presente controla el pasado."

type Line = [Token]
data Token = Word String | Blank | HypWord String
             deriving (Eq, Show)

-- Returns line equivalent of a string with punctuation on words
string2line :: String -> Line
string2line text = map Word (words text)

-- Returns string equivalent of a line without starting or ending spaces
line2string :: Line -> String
line2string [] = ""
line2string line = List.intercalate " " (map token2string (removeOuterSpaces line))

-- Returns string equivalent of a token
token2string :: Token -> String
token2string (Word text) = text
token2string (Blank) = ""
token2string (HypWord text) = text ++ "-"

-- Returns line without starting or ending spaces (TODO: should be made recursive)
removeOuterSpaces :: Line -> Line
removeOuterSpaces line
    | List.length line >= 2 = removeEndingSpace (List.last line) (removeStartingSpace (List.head line) line)
    | List.length line == 1 = removeStartingSpace (List.head line) line
    | otherwise = line

removeStartingSpace :: Token -> Line -> Line
removeStartingSpace (Blank) line = List.drop 1 line
removeStartingSpace _ line = line

removeEndingSpace :: Token -> Line -> Line
removeEndingSpace (Blank) line = List.init line
removeEndingSpace _ line = line

-- Returns length of a token
tokenLength :: Token -> Int
tokenLength (Word text) = List.length text
tokenLength (Blank) = 1
tokenLength (HypWord text) = (List.length text) + 1

-- Returns length of a line without considering starting or ending spaces
lineLength :: Line -> Int
lineLength line = List.length (line2string line)

-- Returns two lines separated at a specified length without starting and ending spaces
breakLine :: Int -> Line -> (Line, Line)
breakLine _ [] = ([], [])
breakLine len line = (List.take tokensFit cleanLine, List.drop tokensFit cleanLine) 
    where tokensFit = maxTokensFit (len + 1) cleanLine 
          cleanLine = removeOuterSpaces line

-- Returns the amount of tokens that fit in a specified length + 1 considering spaces
maxTokensFit :: Int -> Line -> Int
maxTokensFit _ [] = 0
maxTokensFit len (x:xs)
    | tokenSpaceLen <= len = 1 + maxTokensFit (len - tokenSpaceLen) xs
    | otherwise = 0
    where tokenSpaceLen = (tokenLength x) + 1