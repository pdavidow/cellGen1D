{-# LANGUAGE InstanceSigs #-} 
{-# LANGUAGE FlexibleContexts #-} 

module Digit
    ( Digit(..)   
    , seedRow
    )
    where
        
import Data.Array (elems, listArray) 
import CellGen1D (Cell, CellRow(..), Pattern(..), isWrap, isTerminalRow, genCell, seedRow, toString)
import RowLength (RowLength, getLength)  
import System.Random (newStdGen, randoms)


data Digit = D0 | D1 
    deriving (Eq, Show)        


isZero :: Digit -> Bool
isZero d =
    d == D0


isOne :: Digit -> Bool
isOne d = 
    d == D1
 

instance Cell Digit
    where

    isWrap :: Digit -> Bool
    isWrap _ =
        True

   
    isTerminalRow :: CellRow Digit -> Bool  
    isTerminalRow (CellRow row) =
        (all isZero ds) || (all isOne ds)  
            where ds = elems row


    genCell :: Pattern Digit -> Digit 
    genCell (Pattern t) =
        case t of
            (D1, D1, D1) -> D0
            (D1, D1, D0) -> D1
            (D1, D0, D1) -> D1
            (D1, D0, D0) -> D0
            (D0, D1, D1) -> D1
            (D0, D1, D0) -> D1
            (D0, D0, D1) -> D1
            (D0, D0, D0) -> D0


    seedRow :: RowLength -> IO (CellRow Digit)
    seedRow x = do
        gen <- newStdGen  
        let n = getLength x
        let bs = take n $ randoms gen :: [Bool] -- reuse Bool  
        let ds = map (\b -> if b then D1 else D0) bs -- arbitrary
        pure $ CellRow $ listArray (0, n-1) ds        


    toString :: Digit -> String 
    toString x =       
        case x of
            D0 -> "0"
            D1 -> "1"