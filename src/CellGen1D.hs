{-# LANGUAGE ScopedTypeVariables #-} 

module CellGen1D
    ( CellRow(..)
    , Pattern(..)
    , genRow
    , genRows
    , rowToString
    , rowsToString
    , toCells
    , toRow
    )
    where

import Data.Array (Array, ( ! ), elems, listArray)  
import RowLength (RowLength, getLength) 
import Lib (iterateIf)

newtype CellRow a = CellRow (Array Int a) -- zero based
newtype Pattern a = Pattern (a, a, a)


rowToString :: (a -> String) -> CellRow a -> String
rowToString toString x =
    concatMap toString $ toCells x  

    
rowsToString :: (a -> String) -> [CellRow a] -> String
rowsToString f rs = do
    concatMap (\r -> rowToString f r ++ "\n")  rs   


toCells :: CellRow a -> [a]
toCells (CellRow x) =
    elems x  


toRow :: RowLength -> [a] -> CellRow a
toRow rowLength cs =
    CellRow $ listArray (0, n - 1) cs
        where n = getLength rowLength 


genRow :: forall a. (a -> Bool) -> (Pattern a -> a) -> RowLength -> CellRow a -> CellRow a
genRow isWrap genCell rowLength (CellRow row) =
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


genRows :: (CellRow a -> Bool) -> (a -> Bool) -> (Pattern a -> a) -> RowLength -> CellRow a -> [CellRow a] 
genRows isTerminalRow g h rowLength r = 
    iterateIf isTerminalRow (genRow g h rowLength) r            