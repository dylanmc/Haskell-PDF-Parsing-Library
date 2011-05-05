module Main(main) where

import Text.PDF.Types
import Text.PDF.Document
import Text.PDF.Parser
import Text.PDF.Transformations
import System.IO


-- Run this and you get a foo.pdf and a watermarked.pdf, which 
-- smoke-test a good fraction of the library. TODO: some quickcheck and HUnit tests
 
main :: IO ()
main = do
    buildAndWriteFile "foo.pdf" 10
    watermarkFile "foo.pdf" "watermarked.pdf" watermarkPDF
    reversePagesInFile "watermarked.pdf" "reversed.pdf"

filterPDF :: (PDFDocumentParsed -> PDFDocumentParsed) -> PDFContents -> PDFObjectTreeFlattened
filterPDF filterDocument cnts = flattenDocument (unDigestDocument (filterDocument (digestDocument (explodePDF (parseContents cnts)))))
    
reversePagesInFile :: String -> String -> IO ()
reversePagesInFile inName outName = do
    inString <- readFile inName
    let filtered = filterPDF reversePages (PDFContents inString)
    outFile <- openFile outName WriteMode
    _ <- printFlatTree outFile filtered
    hClose outFile
    
buildAndWriteFile :: String -> Int -> IO ()
buildAndWriteFile outName numPages = do
    let firstDoc = (buildDoc numPages)
    let flattened = flattenDocument (unDigestDocument firstDoc)
    outFile <- openFile outName WriteMode
    _ <- printFlatTree outFile flattened
    hClose outFile
    return ()    

buildPage :: String -> Int -> PDF ()
buildPage msg i = do
    beginPage
    moveTo 100 100
    setFont "Helvetica" "F1" 24
    printString (msg ++ (show i))
    endPage
    
buildDoc :: Int -> PDFDocumentParsed
buildDoc i = rundoc $ do
    mapM_ (buildPage "Hello World ") [1..i]
    endDocument

-- This is cheating because it references "/F1", which is a name for a font
-- I know the "foo.pdf" document uses. TODO: build a watermarking function
-- that lets you choose your font, and adds it to the page's resources dictionary
watermarkPDF :: [Char]
watermarkPDF = "BT  100 50 Td /F1 12 Tf(Hello Watermark!) Tj ET"

watermarkFile :: String -> String -> String -> IO ()
watermarkFile inName outName watermarkString = do
    inString <- readFile inName
    let watermarked = filterPDF (watermarkDocument watermarkString) (PDFContents inString) 
    outFile <- openFile outName WriteMode
    _ <- printFlatTree outFile watermarked   
    hClose outFile

