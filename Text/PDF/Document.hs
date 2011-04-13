--------------------------------------------------------------------
-- |
-- Module      : Text.PDF.Document
-- Description : Functions for manipulating PDF content.
-- Copyright   : (c) Dylan McNamee, 2008, 2009
-- License     : BSD3
--
-- Maintainer: Dylan McNamee <dylan@galois.com>
-- Stability : provisional
-- Portability: portable
--
-- Data functions for constructing PDF documents in Haskell.
--------------------------------------------------------------------
module Text.PDF.Document where

import Data.Map as Map
import qualified Data.Traversable as T
import qualified Control.Monad.State as State
import Text.PDF.Types
import Text.PDF.Utils
-- import Text.PDF.Parser
import System.IO
import Data.Maybe

header14 :: String
header14 = "%PDF-1.4\n"

newPage :: PDFObject -> PDFBox -> PDFDictionaryMap -> PDFPageParsed
newPage stream mbox rscDict = PDFPageParsed {
        contents = stream,
        resources = rscDict,
        mediaBox = mbox
    }

-- appendPage pageTree newPage returns a new Page Tree with newPage appended to the end
appendPage :: PDFObject -> PDFObject -> PDFObject
appendPage (PDFArray a) newPage = PDFArray (a ++ [newPage])
appendPage x y = (PDFArray [(PDFError "BadCallToAppendPage with:"), x, y])
-- eventually want to balance the tree of pages

-- deletePage pageTree n returns a new Page Tree with the nth page deleted
deletePage :: PDFObject -> Int -> PDFObject
deletePage _ _ = undefined

-- insertPage pageTree n newPage returns a new Page Tree with newPage inserted at page N
insertPage :: PDFObject -> Int -> PDFObject -> PDFObject
insertPage _ _ _ = undefined

-- build a simple page tree from an array of page object references
pageTreeFromArray :: [PDFObject] -> PDFObject
pageTreeFromArray arr = 
    (PDFDict (fromList[
        ((PDFKey "Type"), (PDFSymbol "Pages")), 
        ((PDFKey "Count"), (PDFInt (length arr))),
        ((PDFKey "Kids"), (PDFArray arr))
    ]))

flattenPage :: PDFPageParsed -> PDFObject -> PDFObject
flattenPage page parentRef = (PDFDict (fromList[
        ((PDFKey "Contents"), (contents page)),
        ((PDFKey "Resources"), (PDFDict (resources page))), -- HRMM: fonts belong inside here. 
        ((PDFKey "Type"), (PDFSymbol "Page")),
        ((PDFKey "MediaBox"), boxToPDFObject (mediaBox page)),
        ((PDFKey "Parent"), parentRef) 
        -- TODO, don't even want the BBox keys for null boxes (cropbox, etc)
    ]))


boxToPDFObject :: PDFBox -> PDFObject
boxToPDFObject (Quad a b c d) = PDFArray [(PDFInt a),(PDFInt b),(PDFInt c),(PDFInt d)]
boxToPDFObject _ = PDFNull
    
-- the Unparsing functions. It's nice how short this is.
showPDFObject :: PDFObject -> String
showPDFObject (PDFString s) =  "(" ++ (escapeString s) ++ ")" 

showPDFObject (PDFSymbol s) =  "/" ++ s 

showPDFObject (PDFDict m) =
    "<<" ++ (foldWithKey showKeyObject "" m) ++ " >>"

showPDFObject (PDFFloat f) = (show f)
showPDFObject (PDFInt i) = (show i)

showPDFObject (PDFArray a) = 
    "[" ++ (foldl showArrayObject "" a) ++ "]"

showPDFObject (PDFReference n g) = (show n) ++ " " ++ (show g) ++ " R"

showPDFObject (PDFStream s) = 
    showPDFObject (PDFDict (fromList [((PDFKey "Length"),(PDFInt (length s)))])) ++
    "\nstream\n" ++ s ++ "\nendstream"
showPDFObject (PDFNull) = "null"
showPDFObject (PDFPageRaw obj) = (showPDFObject obj)
showPDFObject (PDFError str) = (show str)
showPDFObject (PDFComment str) = "%" ++ (show str)
showPDFObject (PDFXObject _) = ("pdfxobject ??")
showPDFObject _ = undefined "can't showPDFObject a non PDFString object (yet)"

showKeyObject :: PDFKey -> PDFObject -> String -> String
showKeyObject (PDFKey key) obj initString = initString ++ 
    "/" ++ key ++ " " ++ (showPDFObject obj) ++ "\n"

showArrayObject :: String -> PDFObject -> String
showArrayObject initString obj = initString ++ 
    (showPDFObject obj) ++ " "

-- Components for creating a PDF document
fontDict :: String -> PDFObject
fontDict name = 
    PDFDict (      -- ... this dict is the definition of it
        fromList [
            ((PDFKey "Type"), (PDFSymbol "Font")),
            ((PDFKey "Subtype"), (PDFSymbol "Type1")),
            ((PDFKey "BaseFont"), (PDFSymbol name)),
            ((PDFKey "Encoding"), (PDFSymbol "MacRomanEncoding"))
        ] )

-- add a mapping between a font shortcut and a font with a name, return new mapping 
addFontToDict :: String -> String -> PDFObject -> PDFObject
addFontToDict shortcut name oldDict = 
    addToDict oldDict shortcut (fontDict name)   
    -- someday silently don't add this if this shortcut's already in the dict
        
type PDF = State.State PDFState
data PDFState = 
    PDFState {
        masterDocument :: PDFDocument,
        streamAccum :: PDFObject, 
        rsrcDict :: PDFObject, 
        fontsDict :: PDFObject,
        pagesArray :: [PDFPageParsed]
        -- pageTreeElt :: PDFObject -- the current node in the page tree
    }
    
-- a wrapper for put to make my silly mistakes result in sensible errors (thanks, sof!)
putPDF :: PDFState -> PDF ()
putPDF s = State.put s

-- TODO: write a "withPage" that does the beginPage/endPage wrapping around a set of
-- PDF() operations (thx SOF, will need your help there...)

-- State.execState takes my state monad, runs it, and returns the result
--   still a bit mysterious, but I think I can handle it.  The rest of the
--   is either monadic, which makes sense, or pure, which makes more sense.
-- It's still this bridge code that throws me a bit.
rundoc :: PDF () -> PDFDocument
rundoc m = d where
    myState = State.execState m newPDFState
    d = masterDocument myState

appendStream :: PDFObject -> String -> PDFObject
appendStream (PDFStream s) ns = PDFStream (s ++ ns)
appendStream _ _ = PDFError "Non-stream argument to appendStream"

newPDFState :: PDFState
newPDFState = PDFState {
        masterDocument = doc,
        streamAccum = PDFStream "",
        rsrcDict = PDFDict (fromList []),
        fontsDict = PDFDict (fromList []),
        pagesArray = []
    } where
        doc = (PDFDocument PDFNull Map.empty )

-- todo: I like putting the media box as an arg to beginPage. HMMMM.
beginPage :: PDF ()
beginPage = do
    myState <- State.get
    let myState' = myState { 
        streamAccum = (PDFStream "BT "), 
        rsrcDict = PDFDict (Map.fromList[])
    } 
    putPDF (myState')

-- at the end of the page, we complete the page's resource dictionary,
-- add the page to the end of the document, and reset streamAccum
-- Andy A-M took one look at this code and said "get out of the Monad, dude"
-- too many lets are an indication that you belong in pure code. Okay!
endPage :: PDF ()
endPage = do
    myState <- State.get
    let s' = appendStream (streamAccum myState) " ET"
    let (PDFDict pageRsrc) = addToDict 
            (addToDict (rsrcDict myState) "ProcSet" globalProcSet) 
            "Font" (fontsDict myState)
    let myState' = myState {
        streamAccum = (PDFStream ""),
        rsrcDict = PDFDict (fromList []),
        fontsDict = PDFDict (fromList []),
        pagesArray = (pagesArray myState) ++ [(newPage s' globalPageBox pageRsrc)]
    }
    putPDF (myState') 

endDocument :: PDF ()
endDocument = do
    myState <- State.get
    let doc' = buildPageTree (masterDocument myState) (pagesArray myState)
    putPDF (myState { masterDocument = doc' })


globalProcSet :: PDFObject    
globalProcSet = PDFArray [(PDFSymbol "PDF"), (PDFSymbol "Text") ]

globalPageBox = Quad 0 0 612 792

-- buildPageTree takes a PDFDocument and an array of PDFPageParsed objects
-- returns a new PDFDocument with a page tree added
--   we modify the PDFPage objects to set their parent values to the newly created parent node(s)
buildPageTree :: PDFDocument -> [PDFPageParsed] -> PDFDocument
buildPageTree doc pgArray = doc'''' where
    (newPagesArray, doc') = buildArrayOfRefs pgArray parentRef doc
    (parentRef, doc'') = addDocObjectGetRef (pageTreeFromArray newPagesArray) doc'
    (_pagesArrayRef, doc''') = addDocObjectGetRef (PDFArray newPagesArray) doc''
    doc'''' = doc''' { 
        catalogDict = pageTreeNode parentRef PDFNull 
    }
    


-- takes an array of Page objects, a parent reference, and a PDFDocument
-- sets the parent of each page object to the parent ref, inserts that modified object 
--    into the document, and returns the array of refs along with the new document
buildArrayOfRefs :: [PDFPageParsed] -> PDFObject -> PDFDocument -> ([PDFObject], PDFDocument)
buildArrayOfRefs [] _ doc = ([], doc)
buildArrayOfRefs (node : rest) parentRef doc = ((nodeRef : restRefs), doc'') where
    (nodeRef, doc') = addDocObjectGetRef newNode doc
    newNode = flattenPage node parentRef
    (restRefs, doc'') = buildArrayOfRefs rest parentRef doc'

-- 
-- The spec says complex objects (arrays, dicts) can either have refs or objects inside them
-- regularize turns everything that's not a simple atomic object into a reference.
regularizeNesting :: PDFObject -> PDFDocument
regularizeNesting root = (PDFDocument newRoot unNestState) where
    (newRoot, unNestState) = State.runState (traverseAndUnNest root) Map.empty

type UnNest = State.State PDFObjectMap
traverseAndUnNest :: PDFObject -> UnNest PDFObject
traverseAndUnNest (PDFArray objs) = PDFArray `fmap` mapM enPointerify objs
traverseAndUnNest (PDFDict myDict) = PDFDict `fmap` T.mapM enPointerify myDict
traverseAndUnNest a = enPointerify a

enPointerify :: PDFObject -> UnNest PDFObject

enPointerify (PDFArray objs) = do
    objs' <- mapM enPointerify objs -- could also use T.mapM here
    reference (PDFArray objs')

enPointerify (PDFDict objs) = do
    objs' <- T.mapM enPointerify objs
    reference (PDFDict objs')

enPointerify o = return o

reference :: PDFObject -> UnNest PDFObject
reference obj = do
    dict <- State.get
    let (dict',ref) = addObjectGetRef dict obj
    State.put dict'
    return ref
    
{- Note to self, above is same as:
 traverseAndUnNest (PDFArray objs) = do
    monadicThingy <- mapM traverseAndUnNest objs
    return (PDFArray monadicThingy)
-}
{-
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
-}

addDocObjectGetRef :: PDFObject -> PDFDocument -> (PDFObject, PDFDocument)
addDocObjectGetRef obj (PDFDocument root oldMap) = (objRef, (PDFDocument root newMap)) where
    (newMap, objRef) = addObjectGetRef oldMap obj

-- this should be in my monad for recursivelyUnNest
addObjectGetRef :: PDFObjectMap -> PDFObject -> (PDFObjectMap, PDFObject)
addObjectGetRef om (PDFReference n g) = (om, (PDFReference n g)) -- don't create refs to refs
addObjectGetRef oldMap pdfobj = (newMap, newRef) where
    newRef = (PDFReference objNum 0)
    newMap = Map.insert objNum pdfobj oldMap
    objNum = (Map.size oldMap + 1)

-- Now for a bunch of imaging operations.
moveTo :: Int -> Int -> PDF ()
moveTo x y = do
    -- (od, PDFStream os, dict, pgs) <- State.get
    myState <- State.get
    let ns = appendStream (streamAccum myState) (" " ++ (show x) ++ 
            " " ++ (show y) ++ " Td ")
    putPDF (myState {streamAccum = ns})

printString :: String -> PDF ()
printString s = do
    myState <- State.get
    let ns = appendStream (streamAccum myState) ("(" ++ s ++ ") Tj")
    putPDF (myState {streamAccum = ns})

setFont :: String -> Int -> PDF ()
setFont name fontSize = do
    myState <- State.get
    let s' = appendStream (streamAccum myState) ("/" ++ name ++ " " ++ (show fontSize) ++ " Tf")
    let fonts' = addFontToDict name name (fontsDict myState)
    let myState' = myState {
        streamAccum = s',
        fontsDict = fonts'
    }
    State.put (myState') 

data ObjectIndices = ObjectIndices [Int] deriving (Show)

-- print the first line, and kick off printing the big "list of objects"
printPDFDocument :: Handle -> PDFDocument -> IO PDFDocument
printPDFDocument h d = do
    let prefixLen = length header14
    hPutStr h (header14)
    ret <- printPDFDocument' h d (ObjectIndices []) prefixLen 1
    return ret

-- empties the PDFObjectList, printing each object as we go, adding
-- its offset in the output file to "ObjectIndices"
printPDFDocument' :: Handle -> PDFDocument -> ObjectIndices -> Int -> Int -> IO PDFDocument
printPDFDocument' 
        h
        (PDFDocument _a objectMap )
        (ObjectIndices ixs)  
        currIx
        objNum = case (mapSize >= objNum) of
            True -> do
                hPutStr h (prefixStr)
                hPutStr h (str)
                hPutStr h (postFixStr)
                printPDFDocument' h (PDFDocument _a objectMap ) (ObjectIndices (ixs ++ [currIx])) newIx (objNum + 1)
            False -> printPDFDocument'' h (PDFDocument _a objectMap ) (ObjectIndices ixs) currIx
        where
            mapSize = Map.size objectMap
            o = fromMaybe (PDFError ("Unable to lookup object # " ++ (show objNum))) (Map.lookup objNum objectMap)
            prefixStr = (show (objNum)) ++ " 0 obj\n"
            str = showPDFObject (o)
            postFixStr = "\nendobj\n"
            newIx = currIx + length str + length prefixStr + length postFixStr

-- empty PDFObjectList -> transition from printing objects to 
-- printing the xref table, which is the list of object indices
-- so this function prints the xref table header, kicks off the 
-- xref table output, then prints the trailer dict and trailer
printPDFDocument'' :: Handle -> PDFDocument -> ObjectIndices -> Int -> IO PDFDocument
printPDFDocument''
        h
        (PDFDocument rootRef _ )
        (ObjectIndices (ixs)) 
        currIx = do
            printXRefIndexes h ixs 0
            let numObjects = 1 + length ixs
            printTrailer h rootRef numObjects currIx 
            return (PDFDocument rootRef Map.empty ) 

printXRefIndexes :: Handle -> [Int] -> Int -> IO () 
printXRefIndexes _ [] _ = do
    return ()

printXRefIndexes h a 0 = do
    hPutStrLn h ("xref\n" ++ "0 " ++ show (1 + length a))
    hPutStrLn h ((padTo "" 10) ++ " 65535 f ")
    printXRefIndexes h a 1

printXRefIndexes h (ix:ixs) n = do
    hPutStrLn h ((padTo (show ix) 10) ++ " 00000 n ")
    printXRefIndexes h ixs (n + 1)

printTrailer :: Handle -> PDFObject -> Int -> Int -> IO ()
printTrailer h rootRef numObjects currIx = do
        hPutStrLn h ("trailer") 
        hPutStrLn h (showPDFObject ((trailerObj numObjects) rootRef ))
        hPutStrLn h ("startxref\n" ++ (show (currIx)))
        hPutStrLn h ("%%EOF")
    where
        trailerObj n rootR = PDFDict 
            (fromList [((PDFKey "Root"), rootR), 
                       ((PDFKey "Size"), (PDFInt n))])




