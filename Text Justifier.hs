import Data.List as List
import System.IO

texto = "Aquel que controla el pasado controla el futuro. Aquel que controla el presente controla el pasado."

type Line = [Token]
data Token = Word String | Blank | HypWord String
             deriving (Eq, Show)


string2line :: String -> Line
string2line text = map Word (words text)

line2string :: Line -> String
line2string line = removeEndingSpace (removeStartingSpace (List.intercalate " " (map token2string line))) 

token2string :: Token -> String
token2string (Word text) = text
token2string (Blank) = ""
token2string (HypWord text) = text ++ "-"

removeStartingSpace :: String -> String
removeStartingSpace text 
    | List.head text == ' ' = List.drop 1 text
    | otherwise = text

removeEndingSpace :: String -> String
removeEndingSpace text 
    | List.last text == ' ' = List.init text
    | otherwise = text