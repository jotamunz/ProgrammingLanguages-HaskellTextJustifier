module TextJustifier where
import Data.List as List
import Data.Map as Map hiding (map)

type Line = [Token]
data Token = Word String | Blank | HypWord String
             deriving (Eq, Show)
type HypMap = Map.Map String [String]

-- Returns if a token is of type blank
isBlank :: Token -> Bool
isBlank (Blank) = True
isBlank _ = False

-- Returns line equivalent of a string with punctuation
-- Applies words to separate string by space
string2line :: String -> Line
string2line text = map Word (List.words text)

-- Returns string equivalent of a line without starting or ending spaces
-- Removes blanks, converts each token to string and intercalates space
line2string :: Line -> String
line2string [] = ""
line2string line = List.intercalate " " (map token2string (removeOuterSpaces line))

-- Returns string equivalent of a token
token2string :: Token -> String
token2string (Word text) = text
token2string (Blank) = ""
token2string (HypWord text) = text ++ "-"

-- Returns line without starting or ending spaces
-- Recursively removes blanks from the beginning and then from the end
removeOuterSpaces :: Line -> Line
removeOuterSpaces line
    | List.null line == False && isBlank (List.head line) == True = removeOuterSpaces (List.drop 1 line)
    | List.null line == False && isBlank (List.last line) == True = removeOuterSpaces (List.init line)
    | otherwise = line

-- Returns length of a token
tokenLength :: Token -> Int
tokenLength (Word text) = List.length text
tokenLength (Blank) = 1
tokenLength (HypWord text) = (List.length text) + 1

-- Returns length of a line without considering starting or ending spaces
lineLength :: Line -> Int
lineLength line = List.length (line2string line)

-- Returns two lines separated at a specified length without starting and ending spaces
-- Removes spaces, calculates the max amount of tokens that fit in the length and splits the line
breakLine :: Int -> Line -> (Line, Line)
breakLine _ [] = ([], [])
breakLine len line = (List.take tokensFit cleanLine, List.drop tokensFit cleanLine) 
    where tokensFit = maxTokensFit (len + 1) cleanLine 
          cleanLine = removeOuterSpaces line

-- Returns the amount of tokens that fit in a specified length + 1 considering spaces
-- Recursively checks if a token and its space fit in a length, increases the counter and subtracts the length
-- All words are considered to have an ending space, the length is added 1 to ignore the line ending space
maxTokensFit :: Int -> Line -> Int
maxTokensFit _ [] = 0
maxTokensFit len (x:xs)
    | tokenSpaceLen <= len = 1 + maxTokensFit (len - tokenSpaceLen) xs
    | otherwise = 0
    where tokenSpaceLen = tokenSpaceLength x

-- Returns length of a token with its consecutive space
tokenSpaceLength :: Token -> Int
tokenSpaceLength (Word text) = List.length text + 1
tokenSpaceLength (Blank) = 1
tokenSpaceLength (HypWord text) = (List.length text) + 2

-- Returns all posible combinations of ordered pairs from a list of strings
mergers :: [String] -> [(String, String)]
mergers strips = mergersAux strips 1

-- Returns all posible combinations of ordered pairs from a list of strings if started at 1
-- Recusively splits a list at the index and concatenates each half into a string forming tuples by incrementing the index. 
mergersAux :: [String] -> Int -> [(String, String)]
mergersAux strips amount 
    | List.length strips <= amount = []
    | otherwise = [(List.intercalate "" (List.take amount strips), List.intercalate "" (List.drop amount strips))] ++ (mergersAux strips (amount + 1))

-- Returns all posible combinations of ordered syllables from a word
-- Separates punctuation, searches for syllables in the dictionary, adds punctuation to last syllable, obtains all pair combinations of syllables, applies tags to each tuple
hyphenate :: HypMap -> Token -> [(Token, Token)]
hyphenate dict (Word text) 
    | Map.notMember cleanText dict == True = []
    | otherwise = [(HypWord (fst x), Word (snd x)) | x <- combinations]
    where combinations = mergers (List.init syllables ++ [List.last syllables ++ punctuation])
          syllables = dict Map.! cleanText
          cleanText = removePunctuation text
          punctuation = extractPunctuation text

-- Returns a string without punctuation
removePunctuation :: String -> String
removePunctuation text = [x | x <- text, not (x `elem` ",.?!:;")] 

-- Returns punctuation of a string
extractPunctuation :: String -> String
extractPunctuation text = [x | x <- text, (x `elem` ",.?!:;")] 

-- Returns a list of all the combinations of lines divided by a Word or a HypWord at a length
-- Calculates the hard division of the line, obtains the hyphen combinations for the division of the line, calculates the hyphen divisions that fit in the remaining length and combines all posibilities
-- The remaining length is reduced by one to consider the extra space before the valid HypWords
lineBreaks :: HypMap -> Int -> Line -> [(Line, Line)]
lineBreaks dict len line 
    | List.null (snd hardSeparation) == True = [hardSeparation] 
    | otherwise = [hardSeparation] ++ separateHypWord hardSeparation validHyphens
    where validHyphens = List.take (maxHypFit (len - (lineLength (fst hardSeparation)) - 1) hyphens) hyphens
          hyphens = hyphenate dict (List.head (snd hardSeparation))
          hardSeparation = breakLine len line

-- Returns the amount of hyphenated word pairs where the HypWord fits in a specified length
-- Recursively checks if the hyphenated part of a word fits in a length for all combinations of hyphens
maxHypFit :: Int -> [(Token, Token)] -> Int
maxHypFit _ [] = 0
maxHypFit len (x:xs)
    | tokenLen <= len = 1 + maxHypFit len xs
    | otherwise = 0
    where tokenLen = tokenLength (fst x)

-- Returns a list of all the combinations of lines divided by a HypWord
-- Recursively adds each HypWord to the end of the first line and the rest of the word at the beginning of the second
separateHypWord :: (Line, Line) -> [(Token, Token)] -> [(Line, Line)]
separateHypWord _ [] = []
separateHypWord hardSeparation (x:xs) = [(fst hardSeparation ++ [fst x], [snd x] ++ List.drop 1 (snd hardSeparation))] ++ (separateHypWord hardSeparation xs)   

-- Returns a line separated by an amount of blanks evenly distributed
-- converts each element in the line to a list, zips it with the sets of blanks for each element and concats all results into a single line
insertBlanks :: Int -> Line -> Line
insertBlanks _ [] = []
insertBlanks amount line = List.concat (List.zipWith (++) lineSeg blankSets)
    where lineSeg = [[x] | x <- line]
          blankSets = createBlankSets amount ((List.length line) - 1)

-- Returns a list with an amount of blanks divided in a certain amount of groups
-- Creates an empty list of the group size, integer divides the blanks into the groups, creates a list with the remainders and fills it with empty to the group size, zips both lists
-- An extra empty is added to make the list the same size as the line it is meant for
createBlankSets :: Int -> Int -> [[Token]]
createBlankSets amount divisor = (List.zipWith (++) integerBlankSets remainderBlankSets) ++ [[]]
    where remainderBlankSets = (List.replicate (snd division) [Blank]) ++ (List.replicate (divisor - snd division) [])
          integerBlankSets = [x ++ (List.replicate (fst division) Blank) | x <- (List.replicate divisor [])]
          division = divMod amount divisor

-- Returns a list of strings broken at a length and based on two flags
-- Converts text to Line, separates the line with Words or with both Words and HypWords, converts each line back to string
-- To adjust the lines, evenly inserts the remaining length with blanks excpet to the last line, converts line piece back to string
separarYalinear :: HypMap -> Int -> Bool -> Bool -> String -> [String]
separarYalinear _ _ _ _ "" = []
separarYalinear dict len separate adjust text 
    | separate == False && adjust == False = map line2string brokenLinesWords
    | separate == False && adjust == True = [line2string (insertBlanks (len - (lineLength x)) x) | x <- List.init brokenLinesWords] ++ [line2string (List.last brokenLinesWords)]
    | separate == True && adjust == False = map line2string brokenLinesHypWords
    | separate == True && adjust == True = [line2string (insertBlanks (len - (lineLength x)) x) | x <- List.init brokenLinesHypWords] ++ [line2string (List.last brokenLinesHypWords)]
    where brokenLinesHypWords = breakAllLinesHypWords dict len line
          brokenLinesWords = breakAllLinesWords len line
          line = string2line text

-- Returns a list of lines broken at a length using only Words
-- Recursively breaks line at a length
breakAllLinesWords :: Int -> Line -> [Line]
breakAllLinesWords _ [] = []
breakAllLinesWords len line = [fst brokenLine] ++ breakAllLinesWords len (snd brokenLine)
    where brokenLine = breakLine len line

-- Returns a list of lines broken at a length using Words or HypWords
-- Recursively breaks line at a length and selects the option with the most length
breakAllLinesHypWords :: HypMap -> Int -> Line -> [Line]
breakAllLinesHypWords _ _ [] = []
breakAllLinesHypWords dict len line = [fst brokenLine] ++ breakAllLinesHypWords dict len (snd brokenLine)
    where brokenLine = List.last (lineBreaks dict len line)
          