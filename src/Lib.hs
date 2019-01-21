module Lib
    ( iterateIf
    )
    where

-- Possibly finite 
iterateIf :: (a -> Bool) -> (a -> a) -> a -> [a]
iterateIf p f x =  
    x : (if p x then [] else iterateIf p f (f x))  