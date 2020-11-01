module Menu where

import Data.List as List
import Data.Map as Map hiding (map)
import System.IO as SysIO
import TextJustifier as Just (HypMap, separarYalinear) 
import Prelude hiding (filter, lookup, map, null) 

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
            handle <- SysIO.openFile fileName SysIO.ReadMode
            newStatus <- loadDict handle (Map.fromList [])
            SysIO.hClose handle
            SysIO.putStrLn ("Diccionario cargado (" ++ (show (List.length (Map.keys newStatus))) ++ " palabras)")
            mainloop newStatus
        "show" -> do
            SysIO.putStrLn (show status)
            mainloop status
        "ins" -> do
            let token = (inpInst !! 1)
            let syllables = (inpInst !! 2)
            newStatus <- addToken status token (List.words [if x == '-' then ' ' else x | x <- syllables])
            mainloop newStatus
        "exit" -> do
            SysIO.putStrLn "Saliendo..."
        _ -> do
            putStrLn $ "Comando desconocido (" ++ command ++ "): '" ++ inpStr ++ "'"
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
        let fileLine = List.words (inpStr)
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


