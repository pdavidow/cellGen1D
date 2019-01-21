{-# LANGUAGE AllowAmbiguousTypes #-} 
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}  

module CellGen1D
    ( Cell
    , CellRow(..)
    , Pattern(..)
    , isWrap
    , isTerminalRow
    , genCell 
    , genRow
    , genRows
    , rowToString
    , rowsToString
    , seedRow 
    , toCells
    , toRow
    , toString 
    )
    where

import Data.Array (Array, ( ! ), elems, listArray)  
import RowLength (RowLength, getLength) 
import Lib (iterateIf)

newtype CellRow a = CellRow (Array Int a) -- zero based
newtype Pattern a = Pattern (a, a, a)


rowToString :: Cell a => CellRow a -> String
rowToString x =
    concatMap toString $ toCells x  

    
rowsToString :: Cell a => [CellRow a] -> String
rowsToString rs = do
    concatMap (\r -> rowToString r ++ "\n")  rs   


toCells :: CellRow a -> [a]
toCells (CellRow x) =
    elems x  


toRow :: Cell a => RowLength -> [a] -> CellRow a
toRow rowLength cs =
    CellRow $ listArray (0, n - 1) cs
        where n = getLength rowLength 


genRows :: Cell a => RowLength -> CellRow a -> [CellRow a] 
genRows rowLength r = 
--     let
--         rs = iterate (genRow rowLength) r
--         (rs1, rs2) = break isTerminal rs
--     in
--         rs1 ++ [head rs2]  
    iterateIf isTerminalRow (genRow rowLength) r   


class Cell a 
    where
    isWrap :: a -> Bool
    isTerminalRow :: CellRow a -> Bool
    genCell :: Pattern a -> a
    seedRow :: RowLength -> IO (CellRow a)    
    toString :: a -> String   

    genRow :: RowLength -> CellRow a -> CellRow a
    genRow rowLength (CellRow row) =
        let
            n = getLength rowLength
            cs = map f $ zip [0 .. (n - 1)] $ elems row

            f :: (Int, a) -> a
            f (i, c) =                     
                let 
                    leftLimit = 0
                    rightLimit = n -1

                    iLeft = 
                        if i' < leftLimit then  
                            if isWrap c then rightLimit else leftLimit
                        else
                            i'
                                where i' = i - 1     

                    iRight  = 
                        if i' > rightLimit then 
                            if isWrap c then leftLimit else rightLimit
                        else 
                            i'
                                where i' = i + 1                   
                in
                    genCell $ Pattern 
                        ( (row ! iLeft) 
                        , (row ! i) 
                        , (row ! iRight) 
                        )
        in
            toRow rowLength cs           