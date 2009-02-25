--------------------------------------------------------------------
-- |
-- Module      : Text.PDF.Parser
-- Description : Reading in and parsing PDF documents
-- Copyright   : (c) Dylan McNamee, 2008
-- License     : BSD3
--
-- Maintainer: Dylan McNamee <dylan@galois.com>
-- Stability : provisional
-- Portability: portable
--
-- Reading in and parsing PDF documents
--------------------------------------------------------------------
module Text.PDF.Parser where

import Text.ParserCombinators.Parsec 
import Text.Read as Read

import Data.Map as Map
import Data.Char as Char
import Data.Array as Array

import Text.PDF.Types
import Text.PDF.Utils

type ObjNum = Int
type FileIndex = Int

data PDFContents = PDFContents String deriving (Show)

digestDocument :: PDFDocument -> PDFDocumentParsed
digestDocument inDoc = PDFDocumentParsed {
        pageList = pages,
        globals = globs
    } where
        globs = extractGlobals inDoc
        pages = getDocumentPages inDoc globs

flattenDocument :: PDFDocumentParsed -> PDFDocument
flattenDocument (PDFDocumentParsed p g) = PDFDocument {
        catalogDict = catalog,
        objectList = objects'
    } where
        catalog = pageTreeNode parentRef PDFNull 
        objects = Prelude.map flattenPage p
        catalogObjectNumber = length objects'
        objects' = objects ++ [parentRef]
        parentRef = PDFReference catalogObjectNumber 0

-- we don't define parent here -- does that matter?
flattenPage :: PDFPageParsed -> PDFObject
flattenPage pp = PDFDict pageDict where
    pageDict = (fromList pageDictList)
    pageDictList = [(PDFKey "Fonts", PDFDict (fonts pp)), (PDFKey "Resources", PDFDict (resources pp)), (PDFKey "Contents", (contents pp))]

extractGlobals :: PDFDocument -> PDFGlobals
extractGlobals d = PDFGlobals {
        rootObject = myRoot
    } where 
        myRoot = PDFString "todo: extract globals" -- error "todo: extract globals"

-- TODO leftoff
-- take a document, extract its page tree and flatten it to a PageList
-- called by digestDocument
getDocumentPages :: PDFDocument -> PDFGlobals -> PDFPageList
getDocumentPages d g = pages where
    catalog = case catalogDict d of
        PDFDict m -> m
        _         -> error "missing/invalid catalog dict"
    root = case Map.lookup (PDFKey "Pages") catalog of
        Nothing                   -> error "missing root in catalog dictionary"
        Just (PDFReference r gen) -> traversePDFReference (PDFReference r gen) d
    pages = flattenPageTree d root
    
-- pageTrees are arrays of either pages, or arrays of pages
-- concatenate them all, recursively

flattenPageTree :: PDFDocument -> PDFObject -> PDFPageList

flattenPageTree doc (PDFArray a) = Prelude.map (parsePage doc) a

--    flattenPageTree doc (PDFArray a) = case head a of 
--        PDFDict d -> Prelude.map (parsePage doc) a
--        _ -> error ("bad page tree element in flattenPageTree" ++ (show a))


flattenPageTree doc (PDFDict d) = case Map.lookup (PDFKey "Kids") d of
        Just (PDFArray k) -> flattenPageTree doc (PDFArray k)
        _            -> error "bad value of Kids array"

flattenPageTree doc o = error "bad page tree in flattenPageTree: "

-- LEFTOFF: TODO: where are Fonts kept? :-) 
-- XXX
parsePage :: PDFDocument -> PDFObject -> PDFPageParsed
parsePage doc (PDFReference r g) = parsePage doc (traversePDFReference (PDFReference r g) doc)
parsePage doc (PDFDict d) = PDFPageParsed {
        fonts = fontsMap,
        resources = resourcesMap,
        contents = contentsObject,
        parent = PDFNull
    } where
        resourcesMap = case Map.lookup (PDFKey "Resources") d of
            Nothing -> Map.empty -- crawl up page tree in this case?
            Just (PDFDict r) -> r 
            Just (PDFReference r g) -> case (traversePDFReference (PDFReference r g) doc) of 
                (PDFDict r) -> r
                _           -> error (" parsePage found a not-dictionary ")
            _ -> error ("bad resources in parsePage" ++ (show d))
        contentsObject = case Map.lookup (PDFKey "Contents") d of 
            Nothing -> PDFNull
            Just c -> c
        fontsMap = case Map.lookup (PDFKey "Fonts") resourcesMap of
            Nothing -> error ("unable to find fonts in " ++ (show resourcesMap))
            Just (PDFDict f) -> f
            _ -> error "bad font in parsePage"

            
parsePage doc (PDFPageRaw pr) = parsePage doc pr

parsePage _ p = error ("internal error: parsePage called on non-dict" ++ (show p))

-- later:
-- getObject :: PDFDocument -> Int -> PDFObject
-- getObject _ _ = undefined

-- Given a list of strings (lines in a file), which are the last few lines of a PDF file
-- return the index of the beginning of the xref table
findXRefOffset :: [String] -> Int
findXRefOffset (first:butFirst)     
                | (startsWith first "startxref") = read (head butFirst)
                | (butFirst == []) = 0
                | True = (findXRefOffset butFirst)
findXRefOffset [] = 0 -- notreached?

-- 
readXRefTable :: ObjNum -> String -> Int -> Map ObjNum FileIndex -> Map ObjNum FileIndex
readXRefTable _ [] _ inMap = inMap
readXRefTable objNum tableString numObjs inMap  
                | objNum > numObjs = inMap
                | offset > 0 = outMap
                | True = inMap -- notneeded? 
          where (offset, tableString')  = parseOffset tableString
                myMap = insert objNum offset inMap
                outMap = readXRefTable (objNum + 1) tableString' numObjs myMap

-- takes a line from the xref table, returns the index and discards the rest of the line
-- returns the rest of the table
parseOffset :: String -> (FileIndex, String)
parseOffset tableStr | True = (offset, restStr')
                where  (offset, restStr) = parseNum tableStr 0
                       restStr' = skipLine restStr 

parseNum :: String -> Int -> (Int, String)
parseNum [] nsf = (nsf, [])
parseNum (c : cs) numSoFar 
                | isSpace c = (numSoFar, cs)
                | isDigit c = parseNum cs (numSoFar * 10 + (digitToInt c))
                | True = (numSoFar, cs)

-- parser functions
run :: Show a => Parser a -> String -> IO ()
run p input
        = case (parse p "" input) of
            Left err -> do
                        putStr "parse error at "
                        print err
            Right x  -> print x

whiteSpace :: Parser String
whiteSpace = many space

charLex :: Parser Char
charLex = do
            whiteSpace
            c <- anyChar
            return c

toInt :: PDFObject -> Int
toInt (PDFInt iv) = iv 
toInt _ = error "toInt on non-numeric PDF object"

topLevelObject :: Parser PDFObject
topLevelObject = try ( do
    objNum <- intLex
    genNum <- intLex
    whiteSpace
    string "obj"
    whiteSpace
    ret <- pdfObject
    whiteSpace
    string "endobj"
    return ret
    )         
              
pdfObject :: Parser PDFObject
pdfObject = pdfStream
    <|>
            pdfDict
    <|>        
            pdfString
    <|>
            pdfReference
    <|>
            pdfSymbol
    <|>
            pdfArray
    <|>
            pdfInt

-- parse a PDF reference which is two ints (objNum, generation) followed by an R by itself
pdfReference :: Parser PDFObject
pdfReference = try ( do
                objNum <- intLex
                genNum <- intLex
                whiteSpace
                testChar <- char 'R'
                many1 space
                let ret = PDFReference (read objNum) (read genNum)
                return ret)

intLex :: Parser String
intLex = do
            whiteSpace
            cs <- many1 digit
            return cs

pdfInt :: Parser PDFObject
pdfInt = do
        ret <- intLex
        return $ PDFInt $ read ret
        
pdfSymbol :: Parser PDFObject
pdfSymbol = try (  do 
                    whiteSpace
                    char '/'
                    c <- letter
                    cs <- many (letter <|> digit)
                    let ret = PDFSymbol (c:cs)
                    return ret
                 )
                 
pdfKey :: Parser PDFKey
pdfKey = try (  do 
                 whiteSpace
                 char '/'
                 c <- letter
                 cs <- many (letter <|> digit)
                 return (PDFKey (c:cs))
              )

top :: Parser p -> Parser p 
top p = do
            many space
            ret <- p
            eof
            return ret

pdfString :: Parser PDFObject
pdfString = (do
            whiteSpace
            char '('
            cs <- many stringChar
            char ')'
            return $ PDFString $ concat cs)
            
stringChar :: Parser String
stringChar =    (do 
                    ret <- noneOf ['\\', ')'] 
                    return [ret] )
            <|> 
                (do 
                    char '\\' 
                    ret <- anyChar
                    return ['\\',ret])

pdfDict :: Parser PDFObject
pdfDict = try (do
            -- trace "startDict" whiteSpace -- printf debugging here...
            string "<<"
            retList <- many keyValuePair
            string ">>"
            let ret = PDFDict (fromList retList)
            -- trace "doneDict" whiteSpace
            return ret)

pdfStreamLen :: Int -> String -> Parser PDFObject
pdfStreamLen 0 completeString = do
            return $ PDFStream completeString
            
pdfStreamLen countRemain stringSofar = try ( do
            char <- anyChar
            ret <- pdfStreamLen (countRemain - 1) (char : stringSofar)
            return ret)
            
pdfStream :: Parser PDFObject
pdfStream = try (do
            lengthDict <- pdfDict
            string "stream"
            -- need to slurp the next <length> bytes
            body <- many anyChar
            string "endstream"
            return $ PDFStream $  body 
            )
            
keyValuePair :: Parser (PDFKey, PDFObject)
keyValuePair =  do
            whiteSpace
            ret <- pdfKey
            -- trace ("key: " ++ (show ret)) whiteSpace
            whiteSpace
            ret' <- pdfObject
            -- trace ("value: " ++ (show ret')) whiteSpace
            whiteSpace
            return (ret, ret')

valueWhitespacePair :: Parser PDFObject
valueWhitespacePair = do
            ret <- pdfObject
            whiteSpace
            return ret
            
pdfArray :: Parser PDFObject
pdfArray = do
            -- trace "array0" 
            char '['
            whiteSpace
            ret <-  many valueWhitespacePair
            char ']'
            whiteSpace
            return $ PDFArray ret

getXRefTable :: PDFContents -> Map ObjNum FileIndex
getXRefTable (PDFContents clx) = readXRefTable 1 restStr'' numObjs Map.empty where
    xrefOffset = findXRefOffset $ lines clx
    fileLen = length $ clx
    fileByteArray = listArray (0,fileLen-1) clx
    xrefTableString = arrayToList (xrefOffset, fileLen - xrefOffset) fileByteArray
    skipXref = skipLine xrefTableString
    (skipZero, restStr) = parseNum skipXref 0
    (numObjs, restStr') = parseNum restStr 0
    restStr'' = skipLine restStr'
    
-- Laboriously extracts nth object's string from a PDF file
-- There are many better ways to do this, but this exercises a lot of the above code...
getObjectString :: PDFContents -> Int -> String
getObjectString (PDFContents clx) objNum = do
    let fileLen = length $ clx
    let fileByteArray = listArray (0,fileLen-1) clx
    let retMap = getXRefTable (PDFContents clx)
    case Map.lookup objNum retMap of
      Nothing -> error ("getObjectString: unbound object number " ++ show objNum)
      Just objOffset -> arrayToList (objOffset, fileLen - objOffset) fileByteArray


parsePDF :: PDFContents -> PDFDocument
parsePDF (PDFContents clx) = PDFDocument {
        catalogDict = rootObject,    -- the catalog dictionary
        objectList = objects         -- objectNum -> PDFObject
    } where
        stringMap = getXRefTable (PDFContents clx)
        rootObject = error "haven't finished parsePDF"
        objects = error "haven't finished parsePDF" -- map (1 .. numObjects) getObjectString (PDFContents clx)


