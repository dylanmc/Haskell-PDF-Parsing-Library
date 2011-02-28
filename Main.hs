module Main(main) where

import Text.PDF.Types
import Text.PDF.Document
import Text.PDF.Parser
import System.IO
 
main :: IO ()
main = buildAndWriteFile "foo.pdf"
-- main = parseAndWriteFile "foo.pdf" "bar.pdf"

buildAndWriteFile :: String -> IO ()
buildAndWriteFile outName = do
    let d = buildDoc
    outFile <- openFile outName WriteMode
    _ <- printPDFDocument outFile d
    hClose outFile
    return ()

buildDoc :: PDFDocument
buildDoc = rundoc $ do
    beginPage 
    moveTo 100 500
    setFont "Helvetica" 24
    printString "Hello World"
    endPage
    beginPage
    moveTo 100 500
    setFont "Times-Roman" 24
    printString "Goodbye World"
    endPage
    endDocument

parseAndWriteFile :: String -> String -> IO ()
parseAndWriteFile inName outName = do
    -- inFileHandle <- openFile inName ReadMode
    inString <- readFile inName
    let contents = PDFContents inString
    let parsed = parsePDF contents
    putStrLn ("done: " ++ show parsed)

        
