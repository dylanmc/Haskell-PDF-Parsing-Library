module Main(main) where

import Text.PDF.Types
import Text.PDF.Document
import Text.PDF.Parser
import Text.PDF.Utils
import Text.PDF.Transformations
import System.IO

 
main :: IO ()
-- main = buildAndWriteFile "foo.pdf" 10
-- main = parseAndWriteFile "foo.pdf" "bar.pdf" -- no transformation done, just tests parse and print
main = watermarkFile "foo.pdf" "watermarked.pdf" watermarkPDF

watermarkPDF = "BT  100 50 Td /F1 12 Tf(Hello Watermark!) Tj ET"

buildAndWriteFile :: String -> Int -> IO ()
buildAndWriteFile outName numPages = do
    let firstDoc = (buildDoc numPages)
    let undigested = unDigestDocument firstDoc
    let d@(PDFObjectTreeFlattened dict objList) = flattenDocument undigested
    outFile <- openFile outName WriteMode
    _ <- printFlatTree outFile d
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

parseAndWriteFile :: String -> String -> IO ()
parseAndWriteFile inName outName = do
    -- inFileHandle <- openFile inName ReadMode
    inString <- readFile inName
    let fileContents = PDFContents inString
    let parsed@(PDFObjectTreeFlattened root _) = parseContents fileContents
    let exploded = explodePDF parsed
    let digested = digestDocument exploded
    let flattened = flattenDocument exploded
    outFile <- openFile outName WriteMode
    _ <- printFlatTree outFile flattened   -- _ <- printFlatTree outFile parsed
    hClose outFile
    -- putStrLn ("before 'sploding: " ++ show root)
    putStrLn ("after 'sploding:" ++  (ppPDFObject 0 exploded))
    -- putStrLn ("after flattening:" ++ (show flattened))
    --putStrLn ("\n\nafter digestion:" ++ (concat (map ppPDFPage (pageList dig))))
    
watermarkFile :: String -> String -> String -> IO ()
watermarkFile inName outName watermarkString = do
    inString <- readFile inName
    let fileContents = PDFContents inString
    let parsed@(PDFObjectTreeFlattened root _) = parseContents fileContents
    let exploded = explodePDF parsed
    let digested = digestDocument exploded
    let watermarked = watermarkDocument digested watermarkString 
    let undigested = unDigestDocument watermarked
    let flattened = flattenDocument undigested
    outFile <- openFile outName WriteMode
    _ <- printFlatTree outFile flattened   
    -- putStrLn ("watermarked: " ++ (ppPDFObject 0 undigested))
    putStrLn ("after flattening:" ++ (show flattened))
    hClose outFile
