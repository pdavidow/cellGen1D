module RowLength
    ( RowLength -- hiding constructor
    , getLength    
    , mkRowLength
    , mkValidRowLength
    )
    where


newtype RowLength = RowLength Int deriving (Eq, Show)


mkRowLength :: Int -> Either String RowLength
mkRowLength n =
    if n > 0 then
        Right $ RowLength n
    else
        Left $ "Row Length must be > 0"


mkValidRowLength :: Int -> RowLength
mkValidRowLength n =
    either error id $ mkRowLength n        


getLength :: RowLength -> Int
getLength (RowLength n) =
    n       