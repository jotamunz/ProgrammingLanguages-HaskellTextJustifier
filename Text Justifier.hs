import Data.List as List
import System.IO

texto = "Aquel que controla el pasado controla el futuro. Aquel que controla el presente controla el pasado."

type Line = [Token]
data Token = Word String | Blank | HypWord String
             deriving (Eq, Show)

-- Returns token list equivalent of a string with punctuation on words
string2line :: String -> Line
string2line text = map Word (words text)

-- Returns string equivalent of a token list without starting or ending spaces
line2string :: Line -> String
line2string [] = ""
line2string line = removeEndingSpace (removeStartingSpace (List.intercalate " " (map token2string line))) 

-- Returns string equivalent of a token
token2string :: Token -> String
token2string (Word text) = text
token2string (Blank) = ""
token2string (HypWord text) = text ++ "-"

removeStartingSpace :: String -> String
removeStartingSpace text 
    | List.null text == False && List.head text == ' ' = List.drop 1 text
    | otherwise = text

removeEndingSpace :: String -> String
removeEndingSpace text 
    | List.null text == False && List.last text == ' ' = List.init text
    | otherwise = text

-- Returns length of a token
tokenLength :: Token -> Int
tokenLength (Word text) = List.length text
tokenLength (Blank) = 1
tokenLength (HypWord text) = (List.length text) + 1

-- Returns length of a token list without considering starting or ending spaces
lineLength :: Line -> Int
lineLength line = List.length (line2string line)