--------------------------------------------------------------------
-- |
-- Module      : Text.PDF.Parser
-- Description : Reading and parsing PDF documents
-- Copyright   : (c) Dylan McNamee, 2008 - 2011
-- License     : BSD3
--
-- Maintainer: Dylan McNamee <dylan@galois.com>
-- Stability : provisional
-- Portability: portable
--
-- Parsing PDF documents
--------------------------------------------------------------------
module Text.PDF.Parser where

import Text.ParserCombinators.Parsec 
-- import Text.Read as Read

import Data.Map as Map
import Data.Char as Char
import Data.Array as Array
-- import Data.Maybe

import Text.PDF.Types hiding ( catalogDict )
import qualified Text.PDF.Types as T ( catalogDict )
import Text.PDF.Utils

-- import Debug.Trace

type ObjNum = Int
type FileIndex = Int

data PDFContents = PDFContents String deriving (Show)

-- From 30,000 feet, Parsing a PDF file goes like this:
--  * find the xref table 
--  * turn the xref table into a map from integers to PDF Objects
--  * find and parse the trailer dictionary
--  * starting with the catalog dict (pointed to by /Root in the trailer dictionary), 
--    recurse through complex objects, parsing sub- and refered-to sub-objects
-- This results in a big tree of objects, no pointers, ready for digesting, manipulating, or flattening.
-- Digesting a PDF file goes like this:
--  * use the trailer dictionary to find and parse the page tree
--  * use the page tree to build a list of pages
--  * for each page, parse it into a PDFPage structure

parseContents :: PDFContents -> PDFObjectTreeFlattened
parseContents pdfContents = PDFObjectTreeFlattened {
        T.catalogDict = catalogObj,                        -- the root, extracted from the catalog
        objectList = parsedObjectMap                     -- objectNum -> PDFObject
    } where
        (xrefEntries, (PDFDict trailerDict)) = getXRefTable pdfContents
        parsedObjectMap = enMapify $ Prelude.map parseObject' objectStrings
        objectStrings = Prelude.map getObjectString' [1..(Map.size xrefEntries)] -- cheating here - flatten instead
        getObjectString' = getObjectString pdfContents xrefEntries
        parseObject' objStr = case (parse topLevelObject "" objStr) of
            Left err -> error ("malformed top-level PDF object: *** " ++ objStr ++ "ERR:" ++ (show err) ++ "***\n")
            Right obj -> obj
        catalogObj = case (Map.lookup (PDFKey "Root") trailerDict) of
            Just (PDFReference n _) -> (parsedObjectMap Map.! n) 
            Just rd@(PDFDict _) -> rd
            Just er -> error ("bad value for Root object in catalog dictionary" ++ (show er))
            _ -> error ("no Root key/value in catalog dictionary: " ++ (show trailerDict))

explodePDF :: PDFObjectTreeFlattened -> PDFTreeExploded
explodePDF (PDFObjectTreeFlattened rootObject objects) = (recursivelyParse objects rootObject)

enMapify :: [a] -> Map Int a
enMapify objList = fromList (zip [1..(length objList)] objList)

recursivelyParse :: (Map Int PDFObject) -> PDFObject -> PDFObject
recursivelyParse _ (PDFString s) = (PDFString s)
recursivelyParse _ (PDFSymbol s) = (PDFSymbol s)
recursivelyParse _ (PDFInt i)    = (PDFInt i)

-- would like to just do Map.map, but there are cycles in the "tree" caused by those ding-dang
-- parent pointers, so filter them out first.
recursivelyParse objectMap (PDFDict d) =  (PDFDict (Map.map (recursivelyParse objectMap) d')) where
    d' = Map.filterWithKey (\k _ -> isn'tParent k) d
    isn'tParent (PDFKey "Parent") = False
    isn'tParent _ = True
    
-- trevor says "you could use an exception transformer" here.
recursivelyParse objectMap (PDFReference n _) = recursivelyParse objectMap (objectMap Map.! n) 
recursivelyParse _         (PDFStream s) = (PDFStream s) -- though some funky PDFs could put the length as a reference
recursivelyParse objectMap (PDFArray a) =  (PDFArray (Prelude.map (recursivelyParse objectMap) a))
    
-- any remaining ones better not be recursively defined, because:
recursivelyParse _ o = o

{-enMapify :: Int -> [a] -> Map Int a -> Map Int a
enMapify _ [] inMap = inMap
enMapify nextKey [first:rest] = enMapify (nextKey+1) rest (insert nextKey first inMap) -}

digestDocument :: PDFTreeExploded -> PDFDocumentParsed
digestDocument inDoc = PDFDocumentParsed {
        pageList = pages
        -- , globals = globs
    } where
        globs = undefined -- extractGlobals inDoc
        pages = Prelude.map parsePage (flattenPageTree inDoc globs)

extractGlobals :: PDFObjectTreeFlattened -> PDFGlobals
extractGlobals _d = PDFGlobals {
    } where 
        myRoot = PDFString "todo: extract globals" -- error "todo: extract globals"

parsePage :: PDFObject -> PDFPageParsed
parsePage (PDFDict pageDict) = PDFPageParsed {
        resources = resourcesDict,
        contents  = cstream,
        mediaBox  = mediab,
        cropBox   = cropb
    } where
        resourcesDict = case Map.lookup (PDFKey "Resources") pageDict of
            Just rd@(PDFDict resDict) -> resDict
            _ -> error "Page missing resources dictionary in parsePage"
        {- fontsDict = case Map.lookup (PDFKey "Font") resourcesDict of
            Just fd@(PDFDict fontDict) -> fontDict
            _ -> Map.empty -}
        cstream = case Map.lookup (PDFKey "Contents") pageDict of
            Just st@(PDFStream s) -> st
            _ -> emptyStream
        mediab = case Map.lookup (PDFKey "MediaBox") pageDict of
            Just (PDFArray list1) -> parseBBox list1
            _ -> NullBox
        cropb = case Map.lookup (PDFKey "CropBox") pageDict of
            Just (PDFArray list2) -> parseBBox list2
            _ -> NullBox
parsePage _ = error "page is not a dictionary in parsePage(!?)"

parseBBox :: [PDFObject] -> PDFBox
parseBBox ((PDFInt a): ((PDFInt b): ((PDFInt c): ((PDFInt d): [])))) = Quad a b c d
parseBBox _ = NullBox -- throw an error someday?

flattenPageTree :: PDFTreeExploded -> PDFObject -> [PDFObject]
flattenPageTree root@(PDFDict catalogDict) _ = flattenPageTree' root
flattenPageTree _ _ = error "page node is not a dictionary in flattenPageTree"

-- takes a Page tree node that is either of Type "Page" or "Pages" and collapses
-- it into an array of Pages
-- TODO: test it on a non-flat page tree. Bummer, need to shift to ByteStreams,
-- because "Real PDFs are not strings, but rather are are binary files". Shoot!
flattenPageTree' :: PDFObject -> [PDFObject]
flattenPageTree' (PDFArray arr) = arr
flattenPageTree' obj@(PDFDict d)    = case Map.lookup (PDFKey "Type") d of
    Just (PDFSymbol "Pages") -> listOfKids where
        kidTrees = case Map.lookup (PDFKey "Kids") d of 
            Just (PDFArray kidArray) -> kidArray
            _ -> error "wonky Pages node in Page Tree"
        listOfKids = concat (Prelude.map flattenPageTree' kidTrees) 
    Just (PDFSymbol "Page") -> [obj]
    Just (PDFSymbol "Catalog") -> flattenedTree where
        flattenedTree = case Map.lookup (PDFKey "Pages") d of
            Just dict@(PDFDict _id) -> flattenPageTree' dict
            _ -> error "bad missing /Pages key in catalog dict"
    _ -> error ("gak: neither Page nor Pages in flattenPageTree': " ++ (ppPDFObject 0 obj))
flattenPageTree' _ = [(PDFError "how did this get into digestPageTree")]

-- parsec functions
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
    _objNum <- intLex
    _genNum <- intLex
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
replicateM iters parser = loop iters
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
            streamLen <- case Map.lookup (PDFKey "Length") lengthDict of
                Just (PDFInt i) -> return i
                Just _ -> fail "non-integer value for Length in a PDF Stream"
                _ -> fail "attempted to parse Stream, couldn't find Length"
            _ <- string "stream"
            _ <- newline
            body <- replicateM streamLen anyChar
            _ <- newline
            _ <- string "endstream"
            -- need to slurp the next <length> bytes
            -- body <- many anyChar -- or could use the value from lengthDict
            -- nice try but no go: body <- anyChar `endBy` string "endstream" -- or could use the value from lengthDict
            -- replicateM -- thanks Trevor! 
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

-- end of parsec functions

-- Parsing the trailer and XRef table
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
-- Read the XRef table from the PDFContents. This table maps 
--   integer object indices to the byte offset of the n'th PDF Object in the document
getXRefTable :: PDFContents -> (Map ObjNum FileIndex, PDFObject) -- returns the trailer dict and the Root Object
getXRefTable (PDFContents clx) = (readXRefTable 1 restStr'' numObjs Map.empty, catalogDict) where
    (_, lastFewLines) = splitAt trailerGuess $ lines clx
    xrefOffset = findXRefOffset $ lastFewLines
    fileLen = length $ clx
    fileByteArray = listArray (0,fileLen-1) clx
    xrefTableString = arrayToList (xrefOffset, fileLen - xrefOffset) fileByteArray
    sanityCheck = startsWith xrefTableString "xref"
    skipXref = case sanityCheck of 
        True -> skipLine xrefTableString
        False -> error "Xref offset incorrect"
    (_skipZero, restStr) = parseNum skipXref 0
    (numObjs, restStr') = parseNum restStr 0
    restStr'' = skipLine restStr'
    trailerGuess = 15
    -- extract the trailer dict, and from that get the Document Catalog 
    catalogStr = skipPast "trailer" lastFewLines -- sure there's a prelude equivalent... todo
    catalogDict = case (parse pdfObject "" (foldl (++) "" catalogStr)) of
        Left err -> error ("malformed Trailer Dictionary *** " ++ (show catalogStr) ++ "ERR:" ++ (show err) ++ "***\n")
        Right (PDFDict d) -> PDFDict d
        Right obj -> error ("Incorrect trailer object type" ++ (show obj)) 

skipPast :: String -> [String] -> [String]
skipPast skipMe [] = error $ "Unable to locate " ++ skipMe
skipPast skipMe (firstLine:restLines) 
    | (startsWith firstLine skipMe) = restLines
    | True = skipPast skipMe restLines

-- Given a list of strings (lines in a file), which are the last few lines of a PDF file
-- return the index of the beginning of the xref table
findXRefOffset :: [String] -> Int
findXRefOffset (first:butFirst)     
                | (startsWith first "startxref") = read (head butFirst)
                | (butFirst == []) = error "Unable to find offset of xref table"
                | True = (findXRefOffset butFirst)
findXRefOffset [] = 0 -- notreached?


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

