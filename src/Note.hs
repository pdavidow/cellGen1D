{-# LANGUAGE InstanceSigs #-} 
{-# LANGUAGE FlexibleContexts #-} 

module Note
    ( Note(..)
    , seedRow
    )
    where
        
import Data.Ix (Ix, range, rangeSize)        
import Data.Array (elems, listArray) 
import CellGen1D (Cell, CellRow(..), Pattern(..), isWrap, isTerminalRow, genCell, seedRow, toString)
import RowLength (RowLength, getLength)  
import System.Random (newStdGen, randomRs)


data Note = A | B | C | D | E | F | G 
    deriving (Eq, Ix, Ord, Show)        


noteRange :: [Note]
noteRange = 
    range (A, G)


instance Cell Note
    where

    isWrap :: Note -> Bool
    isWrap x =
        case x of
            A -> True
            B -> True
            C -> True
            _ -> False

   
    isTerminalRow :: CellRow Note -> Bool  
    isTerminalRow (CellRow row) =
        (head ns == A) || (last ns == G) 
            where ns = elems row


    genCell :: Pattern Note -> Note 
    genCell (Pattern t) =
        case t of
            (A, _, _) -> B
            (B, _, _) -> C
            (C, _, _) -> D
            (D, _, _) -> E
            (E, _, _) -> F
            (F, _, _) -> G
            (G, _, _) -> A


    seedRow :: RowLength -> IO (CellRow Note)
    seedRow x = do
        gen <- newStdGen  
        let n = getLength x
        let is = take n $ randomRs (0, rangeSize (A,G) - 1) gen :: [Int]  
        let xs = map (\i -> noteRange !! i) is
        pure $ CellRow $ listArray (0, n-1) xs        


    toString :: Note -> String 
    toString x =       
        show x