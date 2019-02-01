module Main 
    where

import System.IO   
import CellGen1D (CellRow, Pattern, genRows, rowsToString)
import RowLength (RowLength, mkRowLength)
import Digit (Digit, genCell, isTerminalRow, isWrap, seedRow, toString) 
import Note (Note, genCell, isTerminalRow, isWrap, seedRow, toString) 

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
                                r <- Digit.seedRow rowLength
                                genDisplay Digit.isTerminalRow Digit.isWrap Digit.genCell Digit.toString rowLength r

                            Tagged_Note -> do
                                r <- Note.seedRow rowLength
                                genDisplay Note.isTerminalRow Note.isWrap Note.genCell Note.toString rowLength r
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


genDisplay :: (CellRow a -> Bool) -> (a -> Bool) -> (Pattern a -> a) -> (a -> String) -> RowLength -> CellRow a -> IO ()        
genDisplay f g h i rowLength r = do
    let rs = genRows f g h rowLength r
    putStrLn ""
    displayRows i rs


displayRows :: (a -> String) -> [CellRow a] -> IO ()
displayRows f rs = do
    let (prefix, suffix) = splitAt maxCount rs
    putStrLn $ rowsToString f prefix

    if null suffix then
        pure ()
    else
        seeMore f suffix


seeMore :: (a -> String) -> [CellRow a] -> IO ()    
seeMore f rs = do
    putStrLn "\nSee more?: (0 for no, 1 for yes)" -- todo String or Char
    eof <- isEOF
    if not eof
        then do   
            inpStr <- getLine
            n <- readIO inpStr :: IO Int 

            if n == 1 then do
                putStrLn ""
                displayRows f rs
            else if n == 0 then
                pure () 
            else do
                putStrLn "\nInvalid response"
                seeMore f rs
        else 
            pure ()         

