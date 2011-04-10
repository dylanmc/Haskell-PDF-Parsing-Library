module Main(main) where

import Text.PDF.Types
import Text.PDF.Document
import Text.PDF.Parser
import System.IO
 
main :: IO ()
main = buildAndWriteFile "fooo.pdf" 
-- main = parseAndWriteFile "/Users/dylanjames/Documents/philosophy/Lisi-theoryOfEverything-0711.0770.pdf" "undefined"
-- main = parseAndWriteFile "foo.pdf" "bar.pdf"

buildAndWriteFile :: String -> IO ()
buildAndWriteFile outName = do
    let d = buildDoc
    outFile <- openFile outName WriteMode
    _ <- printPDFDocument outFile d
    hClose outFile
    return ()    

buildPage :: String -> Int -> PDF ()
buildPage msg i = do
    beginPage
    moveTo 100 500
    setFont "Helvetica" 24
    printString (msg ++ (show i))
    endPage
    
buildDoc :: PDFDocument
buildDoc = rundoc $ do
    mapM_ (buildPage "Hello World ") [1..10]
    endDocument

parseAndWriteFile :: String -> String -> IO ()
parseAndWriteFile inName outName = do
    -- inFileHandle <- openFile inName ReadMode
    inString <- readFile inName
    let fileContents = PDFContents inString
    let parsed@(PDFDocument root _) = parsePDF fileContents
    let dig = digestDocument (explodePDF parsed)
    outFile <- openFile outName WriteMode
    _ <- printPDFDocument outFile parsed
    putStrLn ("before 'sploding: " ++ show root)
    putStrLn ("after 'sploding:" ++  (ppPDFObject 0 (explodePDF parsed)))
    putStrLn ("\n\nafter digestion:" ++ (concat (map ppPDFPage (pageList dig))))