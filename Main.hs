module Main(main) where

import Text.PDF.Document
import Text.PDF.Parser ( digestDocument, flattenDocument )
import Text.PDF.Utils
import System.IO
import System.Environment

import Debug.Trace

-- test the monadic document creation code
main = main3 "/Users/dylanjames/Desktop/foo.pdf"

main :: IO ()

mainX = do
    putTraceMsg "1"
    let d = buildDoc
    putTraceMsg ("2")
    let dp = digestDocument d
    putTraceMsg ("3 " ++ (show dp))
    let d' = rotateDocument dp 90
    putTraceMsg "4"
    args <- getArgs
    outFile <- openFile (head args) WriteMode
    putTraceMsg ("5 " ++ (show d'))
    let d'' = flattenDocument d'
    putTraceMsg ("6 " ++ (show d''))
    printPDFDocument stdout d'' 
    return ()

main2 :: String -> IO ()
main2 outName = do
    let d = buildDoc
    let dp = digestDocument d
    let d' = rotateDocument dp 90
    outFile <- openFile outName WriteMode
    printPDFDocument outFile (flattenDocument d')
    return ()

main3 :: String -> IO ()
main3 outName = do
    let d = buildDoc
    outFile <- openFile outName WriteMode
    printPDFDocument outFile d
    return ()

buildDoc = rundoc $ do
        beginPage 
        moveTo 100 500
        setFont "Helvetica" 24
        printString "Hello World"
        endPage
        beginPage
        moveTo 100 100
        setFont "Times-Roman" 24
        printString "Goodbye World"
        endPage
        endDocument
