import Test.Tasty
import Test.Tasty.HUnit

import Data.Array (elems)
import CellGen1D (CellRow(..), genRows, rowsToString, toCells, toRow) 
import RowLength (mkValidRowLength)
import Digit (Digit(..)) 
import Note (Note(..))

main = defaultMain tests
  
tests :: TestTree
tests = testGroup "Tests" [unitTests] 


unitTests = testGroup "Unit tests" $
    [ testGroup "Digit RowLength 1" $    
        let
            rowLength = mkValidRowLength 1
        in
            [ testCase "1.1" $      
                map toCells (genRows rowLength $ toRow rowLength [D0]) @?=     
                    [ [D0]
                    ]   
                    
            , testCase "1.2" $
                map toCells (genRows rowLength $ toRow rowLength [D1]) @?= 
                    [ [D1]
                    ]  
        ]

    , testGroup "Digit RowLength 2" $      
        let
            rowLength = mkValidRowLength 2
        in
            [ testCase "2.1" $
                map toCells (genRows rowLength $ toRow rowLength [D0,D0]) @?= 
                    [ [D0,D0]
                    ]   

            , testCase "2.2" $
                map toCells (genRows rowLength $ toRow rowLength [D0,D1]) @?= 
                    [ [D0,D1]
                    , [D1,D1]
                    ]  
                    
            , testCase "2.3" $
                map toCells (genRows rowLength $ toRow rowLength [D1,D0]) @?= 
                    [ [D1,D0]
                    , [D1,D1]
                    ]    
                    
            , testCase "2.4" $
                map toCells (genRows rowLength $ toRow rowLength [D1,D1]) @?= 
                    [ [D1,D1]
                    ]                   
            ] 

    , testGroup "Digit,Note RowLength 3" $   
        let
            rowLength = mkValidRowLength 3
        in
            [ testCase "3.1" $
                map toCells (genRows rowLength $ toRow rowLength [D0,D0,D0]) @?= 
                    [ [D0,D0,D0]
                    ]   
                                    
            , testCase "3.2" $
                map toCells (genRows rowLength $ toRow rowLength [D1,D0,D0]) @?= 
                    [ [D1,D0,D0]
                    , [D1,D0,D1]
                    , [D1,D1,D1]                
                    ]                 
                            
            , testCase "3.3" $
                map toCells (genRows rowLength $ toRow rowLength [D0,D1,D0]) @?= 
                    [ [D0,D1,D0]
                    , [D1,D1,D0]
                    , [D1,D1,D1]
                    ]  
                    
            , testCase "3.4 (provided example)" $
                map toCells (genRows rowLength $ toRow rowLength [D0,D0,D1]) @?= 
                    [ [D0,D0,D1]
                    , [D0,D1,D1]
                    , [D1,D1,D1]
                    ]                                   

            , testCase "3.5" $
                map toCells (genRows rowLength $ toRow rowLength [D1,D1,D1]) @?= 
                    [ [D1,D1,D1]
                    ]  

            , testCase "3.6" $
                map toCells (take 10 $ genRows rowLength $ toRow rowLength [A,B,C]) @?= 
                    [ [A,B,C]
                    ]   
                                    
            , testCase "3.7" $
                map toCells (take 10 $ genRows rowLength $ toRow rowLength [G,G,G]) @?= 
                    [ [G,G,G]             
                    ]  
                                    
            , testCase "3.8" $
                map toCells (take 10 $ genRows rowLength $ toRow rowLength [B,C,D]) @?= 
                    [ [B,C,D]   
                    , [E,C,D]
                    , [F,F,D]
                    , [G,G,G]
                    ]     
                                                        
            , testCase "3.9" $
                map toCells (take 10 $ genRows rowLength $ toRow rowLength [B,B,B]) @?= 
                    [ [B,B,B]   
                    , [C,C,C]
                    , [D,D,D]
                    , [E,E,E]
                    , [F,F,F]
                    , [G,G,G]
                    ]                     
            ]

    , testGroup "Digit RowLength 4" $   
        let
            rowLength = mkValidRowLength 4
        in
            [ testCase "4.1" $
                map toCells (take 10 $ genRows rowLength $ toRow rowLength [D0,D1,D0,D1]) @?= 
                    [ [D0,D1,D0,D1] 
                    , [D1,D1,D1,D1]                                                                                                                                                                                            
                    ]   

            , testCase "4.2" $
                map toCells (take 10 $ genRows rowLength $ toRow rowLength [D0,D1,D1,D1]) @?= 
                    [ [D0,D1,D1,D1]
                    , [D1,D1,D0,D1]
                    , [D0,D1,D1,D1]  
                    , [D1,D1,D0,D1]  
                    , [D0,D1,D1,D1]  
                    , [D1,D1,D0,D1]  
                    , [D0,D1,D1,D1]  
                    , [D1,D1,D0,D1]  
                    , [D0,D1,D1,D1]  
                    , [D1,D1,D0,D1]                                                                                                                                                                                                                                                                              
                    ]  
                    

            , testCase "4.3" $
                map toCells (take 20 $ genRows rowLength $ toRow rowLength [D0,D1,D1,D1]) @?= 
                    [ [D0,D1,D1,D1]
                    , [D1,D1,D0,D1]
                    , [D0,D1,D1,D1]  
                    , [D1,D1,D0,D1]  
                    , [D0,D1,D1,D1]  
                    , [D1,D1,D0,D1]  
                    , [D0,D1,D1,D1]  
                    , [D1,D1,D0,D1]  
                    , [D0,D1,D1,D1]  
                    , [D1,D1,D0,D1] 
                    , [D0,D1,D1,D1]
                    , [D1,D1,D0,D1]
                    , [D0,D1,D1,D1]  
                    , [D1,D1,D0,D1]  
                    , [D0,D1,D1,D1]  
                    , [D1,D1,D0,D1]  
                    , [D0,D1,D1,D1]  
                    , [D1,D1,D0,D1]  
                    , [D0,D1,D1,D1]  
                    , [D1,D1,D0,D1]                                                                                                                                                                                                                                                                                                   
                    ]                       
            ]
        
    ,  testGroup "rowsToString" $                  
        [ testCase "Digit rowsToString" $
            let
                rowLength = mkValidRowLength 3
            in
                rowsToString
                    [ toRow rowLength [D0,D0,D1]
                    , toRow rowLength [D0,D1,D1]
                    , toRow rowLength [D1,D1,D1]
                    ]
                        @?= "001\n011\n111\n"

        , testCase "Note rowsToString" $
            let
                rowLength = mkValidRowLength 3
            in
                rowsToString
                    [ toRow rowLength [A,B,C]
                    , toRow rowLength [G,D,B]
                    , toRow rowLength [C,E,G]
                    ]
                        @?= "ABC\nGDB\nCEG\n"                        
            ]
    ] 