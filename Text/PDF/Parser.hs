--------------------------------------------------------------------
-- |
-- Module      : Text.PDF.Parser
-- Description : Reading in and parsing PDF documents
-- Copyright   : (c) Dylan McNamee, 2008, 2009
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
import Data.Maybe

import Text.PDF.Types
import Text.PDF.Utils

import Debug.Trace

type ObjNum = Int
type FileIndex = Int

data PDFContents = PDFContents String deriving (Show)

-- From 30,000 feet, Parsing a PDF file goes like this:
--  * find the xref table 
--  o turn the xref table into a map from integers to PDF Objects
--  o find and parse the trailer dictionary
--  o use the trailer dictinoary to find and parse the page tree
--  o use the page tree to build a list of pages
--  o for each page, parse it into a PDFPage structure
parsePDF :: PDFContents -> PDFDocument
parsePDF contents = PDFDocument {
        catalogDict = rootObject,                               -- the catalog dictionary
        objectList = Prelude.map parseObject' objectStrings     -- objectNum -> PDFObject
    } where
        (xrefEntries, rootObject) = getXRefTable contents
        objectStrings = Prelude.map getObjectString' [1..(Map.size xrefEntries)]
        getObjectString' = getObjectString contents xrefEntries
        parseObject' objStr = case (parse topLevelObject "" objStr) of
            Left err -> error ("malformed top-level PDF object: *** " ++ objStr ++ "ERR:" ++ (show err) ++ "***\n")
            Right obj -> obj

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
        _                         -> error "malformed catalog dictionary"
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
                (PDFDict r') -> r'
                _            -> error (" parsePage found a not-dictionary ")
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

getPageTree :: PDFDocument -> PDFObject
getPageTree d = traversePDFReference (getPageTreeRef d) d
-- getPageTreeRef :: PDFDocument -> PDFObject
getPageTreeRef (PDFDocument catalogDictRef objList ) = pageTreeRef where
    catalogDictMap = case (traversePDFReference catalogDictRef 
                            (PDFDocument undefined objList )) of
        (PDFDict dict)  -> dict
        foo             -> fromList ([((PDFKey "error"), foo)])
    pageTreeRef = fromMaybe (PDFError (show catalogDictMap)) 
                            (Map.lookup (PDFKey "Pages") catalogDictMap)

-- getPage pageTree n returns the nth PDFPage object in the document
getPage :: PDFObject -> Int -> Maybe PDFObject
getPage (PDFArray []) _ = undefined -- signal an error
getPage (PDFArray (treeNode:restNodes))  n = 
   case (dictLookup "Type" treeNode "bad type in getPage")  of
     (PDFSymbol "Page")
          | n == 0    -> Just treeNode
          | otherwise -> getPage (PDFArray restNodes) (n-1)
     (PDFSymbol "Pages")   ->
          case (dictLookup "Kids" treeNode "no children array in node") of
            (PDFArray childArray) ->
              case (dictLookup "Count" treeNode "invalid Pages node (no count)") of
                (PDFInt countPages) 
                  | n < countPages  -> getPage (PDFArray childArray) (countPages - n) -- go deeper
                  | otherwise       -> getPage (PDFArray restNodes) (n - countPages) -- skip this node
                _ -> Nothing -- "bad count in page tree"
            _ -> Nothing -- "missing child array in page tree"
     _ -> Nothing -- "bad Dict type in getPage"
getPage _ _ = error "unexpected page structure"

-- later:
-- getObject :: PDFDocument -> Int -> PDFObject
-- getObject _ _ = undefined

-- Given a list of strings (lines in a file), which are the last few lines of a PDF file
-- return the index of the beginning of the xref table
findXRefOffset :: [String] -> Int
findXRefOffset (first:butFirst)     
                | (startsWith first "startxref") = read (head butFirst)
                | (butFirst == []) = error "Unable to find offset of xref table"
                | True = (findXRefOffset butFirst)
findXRefOffset [] = 0 -- notreached?

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
            _ <- whiteSpace
            c <- anyChar
            return c

toInt :: PDFObject -> Int
toInt (PDFInt iv) = iv 
toInt _ = error "toInt on non-numeric PDF object"

topLevelObject :: Parser PDFObject
topLevelObject =  do
    objNum <- intLex
    genNum <- intLex
    _ <- whiteSpace
    _ <- string "obj"
    _ <- whiteSpace
    ret <- pdfObject
    _ <- whiteSpace
    _ <- string "endobj"
    return ret        
              
pdfObject :: Parser PDFObject
pdfObject = try pdfStream 
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
                _ <- whiteSpace
                _ <- char 'R'
                _ <- whiteSpace
                let ret = PDFReference (read objNum) (read genNum)
                return ret)

intLex :: Parser String
intLex = do
            _ <- whiteSpace
            cs <- many1 digit
            return cs

pdfInt :: Parser PDFObject
pdfInt = do
        ret <- intLex
        return $ PDFInt $ read ret
     
pdfToken :: Parser String
pdfToken = do
                 c <- letter
                 cs <- many (letter <|> digit <|> char '-' <|> char '_' )
                 return (c:cs)
                        
pdfSymbol :: Parser PDFObject
pdfSymbol =  do 
                    _ <- whiteSpace
                    _ <- char '/'
                    str <- pdfToken
                    let ret = PDFSymbol str
                    return ret
                 
pdfKey :: Parser PDFKey
pdfKey = do 
                 _ <- whiteSpace
                 _ <- char '/'
                 str <- pdfToken
                 return $ PDFKey str

top :: Parser p -> Parser p 
top p = do
            _ <- many space
            ret <- p
            eof
            return ret

pdfString :: Parser PDFObject
pdfString = do
            _ <- whiteSpace
            _ <- char '('
            cs <- many stringChar
            _ <- char ')'
            return $ PDFString $ concat cs
            
stringChar :: Parser String
stringChar =    (do 
                    ret <- noneOf ['\\', ')'] 
                    return [ret] )
            <|> 
                (do 
                    _ <- char '\\' 
                    ret <- anyChar
                    return ['\\',ret])

pdfDict :: Parser PDFObject
pdfDict = do
            -- trace "startDict" whiteSpace -- printf debugging here...
            _ <- whiteSpace
            _ <- string "<<"
            retList <- many1 keyValuePair
            _ <- string ">>"
            let ret = PDFDict (fromList retList)
            -- trace ("doneDict" ++ show ret) whiteSpace
            _ <- whiteSpace
            return ret

replicateM :: Int -> Parser a -> Parser [a]
replicateM count parser = loop count
    where
    loop i | i <= 0 = return []
           | otherwise = do
                a <- parser
                rest <- loop (i-1) 
                return (a : rest)
{-
replicateM i parser = sequence (replicate i parser)
-}
            
pdfStream :: Parser PDFObject
pdfStream = do
            -- _ <- pdfDict
            PDFDict lengthDict <- pdfDict
            length <- case Map.lookup (PDFKey "Length") lengthDict of
                Just (PDFInt i) -> return i
                Just _ -> fail "non-integer value for Length in a PDF Stream"
                _ -> fail "attempted to parse Stream, couldn't find Length"
            _ <- string "stream"
            _ <- newline
            body <- replicateM length anyChar
            _ <- newline
            string "endstream"
            -- need to slurp the next <length> bytes
            -- body <- many anyChar -- or could use the value from lengthDict
            -- nice try but no go: body <- anyChar `endBy` string "endstream" -- or could use the value from lengthDict
            -- thanks Trevor! (not: _ <- string "endstream"
            return $ PDFStream $  body 


            
keyValuePair :: Parser (PDFKey, PDFObject)
keyValuePair =  do
            _ <- whiteSpace
            ret <- pdfKey
            -- trace ("key: " ++ (show ret)) whiteSpace
            _ <- whiteSpace
            ret' <- pdfObject
            -- trace ("value: " ++ (show ret')) whiteSpace
            _ <- whiteSpace
            return (ret, ret')

valueWhitespacePair :: Parser PDFObject
valueWhitespacePair = do
            ret <- pdfObject
            _ <- whiteSpace
            return ret
            
pdfArray :: Parser PDFObject
pdfArray = do
            -- trace "array0" 
            _ <- char '['
            _ <- whiteSpace
            ret <-  many valueWhitespacePair
            _ <- char ']'
            _ <- whiteSpace
            return $ PDFArray ret

-- Read the XRef table from the PDFContents. This table maps 
--   integer object indices to the byte offset of the n'th PDF Object in the document
-- TODO: fill in the root object
getXRefTable :: PDFContents -> (Map ObjNum FileIndex, PDFObject) -- returns the trailer dict and the Root Object
getXRefTable (PDFContents clx) = (readXRefTable 1 restStr'' numObjs Map.empty, rootObject) where
    (_, lastFewLines) = splitAt trailerGuess $ lines clx
    xrefOffset = findXRefOffset $ lastFewLines
    fileLen = length $ clx
    fileByteArray = listArray (0,fileLen-1) clx
    xrefTableString = arrayToList (xrefOffset, fileLen - xrefOffset) fileByteArray
    skipXref = skipLine xrefTableString
    (skipZero, restStr) = parseNum skipXref 0
    (numObjs, restStr') = parseNum restStr 0
    restStr'' = skipLine restStr'
    trailerGuess = 40
    -- extract the trailer dict, and from that get the Document Catalog (aka Root)
    catalogStr = skipPast "trailer" lastFewLines
    rootObject = case (parse pdfObject "" (foldl (++) "" catalogStr)) of
        Left err -> error ("malformed Trailer Dictionary *** " ++ (show catalogStr) ++ "ERR:" ++ (show err) ++ "***\n")
        Right (PDFDict d) -> PDFDict d
        Right obj -> error ("Incorrect trailer object type" ++ (show obj)) 

skipPast :: String -> [String] -> [String]
skipPast skipMe [] = error $ "Unable to locate " ++ skipMe
skipPast skipMe (line:lines) 
    | (startsWith line skipMe) = lines
    | True = skipPast skipMe lines

-- Laboriously extracts nth object's string from a PDF file
-- There are many better ways to do this, but this exercises a lot of the above code...
getObjectString :: PDFContents -> Map ObjNum FileIndex -> Int -> String
getObjectString (PDFContents clx) stringMap objNum = do
    let fileLen = length $ clx
    let fileByteArray = listArray (0,fileLen-1) clx
    case Map.lookup objNum stringMap of
      Nothing -> error ("getObjectString: unbound object number " ++ show objNum)
      Just objOffset -> arrayToList (objOffset, fileLen - objOffset) fileByteArray

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

{- 
  Section 3.6 of the PDF reference (version 1.6) talks about PDF Document structure.
  The Trailer Dict has a Root entry. The root entry points to the Catalog Dictionary.
  The Catalog Dictionary holds the 
    * /Pages -> page tree, 
    * the outline hierarchy, 
    * article threads, 
    * named destinations, and 
    * form data.
-}