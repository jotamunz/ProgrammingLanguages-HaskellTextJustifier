module Menu where

import Data.List as List
import Data.Map as Map hiding (map)
import System.IO as SysIO
import System.Directory as Dir (doesFileExist)
import Text.Read as Read (readMaybe)
import Data.Char as Char (ord)
import Prelude hiding (filter, lookup, map, null) 
import TextJustifier as Just (HypMap, separarYalinear) 

type Status = HypMap

-- Creates an empty status and calls the main loop
main :: IO ()
main = do
    mainloop (Map.fromList [])

-- Recursively recieves a status, extracts command form first word, runs command and produces a new status
mainloop :: Status -> IO ()
mainloop status = do
    SysIO.putStr ">> "
    inpStr <- SysIO.getLine
    let inpInst = List.words inpStr
    let command = inpInst !! 0
    case command of
        "load" -> do
            let fileName = (inpInst !! 1)
            fileExists <- Dir.doesFileExist fileName
            if fileExists == False then do
                SysIO.putStrLn "Archivo no encontrado, intente de nuevo"
                mainloop status
            else do
                handle <- SysIO.openFile fileName SysIO.ReadMode
                newStatus <- loadDict handle (Map.fromList [])
                SysIO.hClose handle
                SysIO.putStrLn ("Diccionario cargado (" ++ (show (List.length (Map.keys newStatus))) ++ " palabras)")
                mainloop newStatus
        "show" -> do
            SysIO.putStrLn (List.unlines [(fst x) ++ " " ++ (List.intercalate "-" (snd x)) | x <- Map.toList status])
            mainloop status
        "ins" -> do
            let token = (inpInst !! 1)
            let syllables = (inpInst !! 2)
            newStatus <- addToken status token (List.words [if x == '-' then ' ' else x | x <- syllables])
            SysIO.putStrLn ("Palabra " ++ token ++ " agregada")
            mainloop newStatus
        "save" -> do 
            let fileName = (inpInst !! 1)
            handle <- SysIO.openFile fileName SysIO.WriteMode
            SysIO.hPutStr handle (unnormalizeText (List.unlines [(fst x) ++ " " ++ (List.intercalate "-" (snd x)) | x <- Map.toList status]))
            SysIO.hClose handle
            SysIO.putStrLn ("Diccionario guardado (" ++ (show (List.length (Map.keys status))) ++ " palabras)")
            mainloop status
        "split" -> do 
            let len = string2int (inpInst !! 1)
            let separate = string2bool (inpInst !! 2)
            let adjust = string2bool (inpInst !! 3)
            if List.null separate || List.null adjust || len == 0
                then SysIO.putStrLn "Valor incorrecto para separar o ajustar (s/n) o para largo de linea (>=1)"
            else do
                let justifiedText = Just.separarYalinear status len (List.head separate) (List.head adjust) (List.unwords (List.drop 4 inpInst))
                SysIO.putStrLn (List.unlines justifiedText)
            mainloop status
        "splitf" -> do 
            let fileName = (inpInst !! 4)
            fileExists <- Dir.doesFileExist fileName
            if fileExists == False then do
                SysIO.putStrLn "Archivo no encontrado, intente de nuevo"
                mainloop status
            else do
                handle <- SysIO.openFile fileName SysIO.ReadMode
                text <- SysIO.hGetContents handle
                let normalizedText = normalizeText text
                let len = string2int (inpInst !! 1)
                let separate = string2bool (inpInst !! 2)
                let adjust = string2bool (inpInst !! 3)
                if List.null separate || List.null adjust || len == 0
                    then SysIO.putStrLn "Valor incorrecto para separar o ajustar (s/n) o para largo de linea (>=1)"
                else do
                    let justifiedText = Just.separarYalinear status len (List.head separate) (List.head adjust) normalizedText
                    if List.length inpInst == 6 then do
                        let fileName = (inpInst !! 5)
                        handle2 <- SysIO.openFile fileName SysIO.WriteMode
                        SysIO.hPutStr handle2 (unnormalizeText (List.unlines justifiedText))
                        SysIO.hClose handle2
                        SysIO.putStrLn ("Texto guardado (" ++ (show (List.length justifiedText)) ++ " lineas)")
                    else SysIO.putStrLn (List.unlines justifiedText)
                SysIO.hClose handle
                mainloop status
        "exit" -> do
            SysIO.putStrLn "Saliendo..."
        _ -> do
            SysIO.putStrLn $ "Comando desconocido (" ++ command ++ "): '" ++ inpStr ++ "'"
            mainloop status

-- Returns the status with a dictionary according to the handle
-- Recursively checks if there are still lines to read, divides the line in two, divides the syllables, adds the key and the syllables list
loadDict :: Handle -> Status -> IO Status
loadDict handle status = do
    allRead <- SysIO.hIsEOF handle
    if allRead
        then return status
    else do
        inpStr <- SysIO.hGetLine handle
        let fileLine = List.words (normalizeText inpStr)
        let newStatus = addTokenAux status (List.head fileLine) (List.words [if x == '-' then ' ' else x | x <- List.last fileLine])
        loadDict handle newStatus

-- Return the status with the token and its syllables added
-- Checks if the token is already in the dictionary, if not adds it
addTokenAux :: Status -> String -> [String] -> Status
addTokenAux status token syllables = 
    if Map.member token status
        then status
    else Map.insert token syllables status

-- Return the status with the token and its syllables added
-- Checks if the token is already in the dictionary, if not adds it
addToken :: Status -> String -> [String] -> IO Status
addToken status token syllables = 
    if Map.member token status
        then return status
    else return (Map.insert token syllables status)

-- Returns a list containing the bool value of a yes or no string, if invalid returns empty
string2bool :: String -> [Bool]
string2bool str
    | str == "s" = [True]
    | str == "n" = [False]
    | otherwise = []

-- Returns the int value of a string, if invalid returns 0
string2int :: String -> Int
string2int str =
    case maybeInt of
        Just n  -> n
        Nothing -> 0
    where maybeInt = Read.readMaybe str :: Maybe Int

-- Returns a string replacing the decomposed ascii for accents with the composed ascii for accents
normalizeText :: String -> String
normalizeText [] = []
normalizeText (x:xs)
    | Char.ord x == 9500 && Char.ord (List.head xs) == 237 = "á" ++ (normalizeText (List.drop 1 xs))
    | Char.ord x == 9500 && Char.ord (List.head xs) == 8976 = "é" ++ (normalizeText (List.drop 1 xs))
    | Char.ord x == 9500 && Char.ord (List.head xs) == 161 = "í" ++ (normalizeText (List.drop 1 xs))
    | Char.ord x == 9500 && Char.ord (List.head xs) == 9474 = "ó" ++ (normalizeText (List.drop 1 xs))
    | Char.ord x == 9500 && Char.ord (List.head xs) == 9553 = "ú" ++ (normalizeText (List.drop 1 xs))
    | otherwise = [x] ++ normalizeText xs

-- Returns a string replacing the composed ascii for accents with the decomposed ascii for accents
unnormalizeText :: String -> String
unnormalizeText [] = []
unnormalizeText (x:xs)
    | x == 'á' = "\9500\237" ++ unnormalizeText xs
    | x == 'é' = "\9500\8976" ++ unnormalizeText xs
    | x == 'í' = "\9500\161" ++ unnormalizeText xs
    | x == 'ó' = "\9500\9474" ++ unnormalizeText xs
    | x == 'ú' = "\9500\9553" ++ unnormalizeText xs
    | otherwise = [x] ++ unnormalizeText xs