{-# LANGUAGE ScopedTypeVariables #-}

module Main 
    where

import System.IO   
import CellGen1D (Cell, CellRow, genRows, rowsToString)
import RowLength (RowLength, mkRowLength)
import Digit (Digit, seedRow) 
import Note (Note, seedRow) 

data Tagged_CellType
    = Tagged_Digit 
    | Tagged_Note 


maxCount :: Int
maxCount = 50 -- arbitrary


main :: IO ()
main = do
    putStrLn "\nEnter row length (0 to quit): "
    eof <- isEOF
    if not eof
        then do            
            inpStr <- getLine
            len <- readIO inpStr :: IO Int

            if len == 0 then
                pure ()
            else do                
                let eiRowLength = mkRowLength len

                case eiRowLength of
                    Right rowLength -> do
                        t <- cellType
                        case t of 
                            Tagged_Digit -> do
                                r :: CellRow Digit <- seedRow rowLength
                                genDisplay rowLength r

                            Tagged_Note -> do
                                r :: CellRow Note <- seedRow rowLength
                                genDisplay rowLength r
                    Left error ->
                        putStrLn error
                main
        else 
            pure () 

 
cellType :: IO Tagged_CellType
cellType = do
    putStrLn "\nWhich cell type ? (1: Digit, 2: Note): "
    eof <- isEOF
    if not eof
        then do            
            inpStr <- getLine
            n <- readIO inpStr :: IO Int
            case n of
                1 -> pure Tagged_Digit 
                2 -> pure Tagged_Note                
                _ -> do
                    putStrLn "\nInvalid response"
                    cellType
    else 
        cellType


genDisplay :: Cell a => RowLength -> CellRow a -> IO ()        
genDisplay rowLength r = do
    let rs = genRows rowLength r
    putStrLn ""
    displayRows rs


displayRows :: Cell a => [CellRow a] -> IO ()
displayRows rs = do
    let (prefix, suffix) = splitAt maxCount rs
    putStrLn $ rowsToString prefix

    if null suffix then
        pure ()
    else
        seeMore suffix


seeMore :: Cell a => [CellRow a] -> IO ()    
seeMore rs = do
    putStrLn "\nSee more?: (0 for no, 1 for yes)" -- todo String or Char
    eof <- isEOF
    if not eof
        then do   
            inpStr <- getLine
            n <- readIO inpStr :: IO Int 

            if n == 1 then do
                putStrLn ""
                displayRows rs
            else if n == 0 then
                pure () 
            else do
                putStrLn "\nInvalid response"
                seeMore rs
        else 
            pure ()         

