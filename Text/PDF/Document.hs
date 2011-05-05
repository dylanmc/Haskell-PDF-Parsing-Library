--------------------------------------------------------------------
-- |
-- Module      : Text.PDF.Document
-- Description : Functions for manipulating PDF content.
-- Copyright   : (c) Dylan McNamee, 2008, 2009, 2011
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
import Text.PDF.Types hiding ( parent, dictMap )
import Text.PDF.Utils
-- import Text.PDF.Parser
import System.IO
import Data.Maybe

header14 :: String
header14 = "%PDF-1.4\n"
 
-- | @unDigestDocument dp@ takes a parsed document and turns all of the parsed objects into generic
-- objects. Most particularly, PDFPageParsed into PDFDict's by inserting the salient page attributes
-- into their stylized key/value pairs (like Contents, MediaBox, etc.)
unDigestDocument :: PDFDocumentParsed -> PDFTreeExploded
unDigestDocument (PDFDocumentParsed parsedPageList {- globals -} ) = (catalogDictFromArray pageArray) where
    (PDFArray pageArray) = PDFArray (Prelude.map unParsePage parsedPageList)

-- | @flattenDocument treeObj@ takes @treeObj@, a nested representation of a PDF document tree, and 
-- flattens it by replacing nesting with PDF references to an object list it accumulates as it goes.
-- In theory, the exploded tree is "very close" to a valid PDF structure, because nesting is 
-- okay according to the spec. However, flattening objects gives us the opportunity to discover
-- identical items and collapse them, and also, for some crazy reason, PDFStreams have to be 
-- referenced inside of a page - they can't be nested.

flattenDocument :: PDFObject -> PDFObjectTreeFlattened
flattenDocument root = (PDFObjectTreeFlattened newRoot unNestState) where
    (newRoot, unNestState) = State.runState (traverseAndUnNest root) Map.empty

newPage :: PDFObject -> PDFBox -> PDFDictionaryMap -> PDFPageParsed
newPage stream mbox rscDict = PDFPageParsed {
        contents = stream,
        resources = rscDict,
        mediaBox = mbox,
        cropBox = mbox
    }

-- | @printFlatTree file objectTree@ is the pretty printer that actually produces valid PDF
-- from @objectTree@ on to the file handle @file@ (when we're lucky)
printFlatTree :: Handle -> PDFObjectTreeFlattened -> IO PDFObjectTreeFlattened
printFlatTree h d@(PDFObjectTreeFlattened _ _) = do
    let prefixLen = length header14
    hPutStr h (header14)
    let d' = enpointerifyRoot d
    ret <- printFlatTree' h d' (ObjectIndices []) prefixLen 1
    return ret

-- appendPage pageTree newPage returns a new Page Tree with newPage appended to the end
appendPage :: PDFObject -> PDFObject -> PDFObject
appendPage (PDFArray a) newPage' = PDFArray (a ++ [newPage'])
appendPage x y = (PDFArray [(PDFError "BadCallToAppendPage with:"), x, y])
-- eventually want to balance the tree of pages

-- deletePage pageTree n returns a new Page Tree with the nth page deleted
deletePage :: PDFObject -> Int -> PDFObject
deletePage _ _ = undefined

-- insertPage pageTree n newPage returns a new Page Tree with newPage inserted at page N
insertPage :: PDFObject -> Int -> PDFObject -> PDFObject
insertPage _ _ _ = undefined

catalogDictFromArray :: [PDFObject] -> PDFObject
catalogDictFromArray objs = 
    (PDFDict (fromList [
        ((PDFKey "Type"), (PDFSymbol "Catalog")),
        ((PDFKey "Pages"), pageTreeFromArray objs) ]))
        
-- build a simple page tree from an array of page object references
-- TODO: build a tree with > depth 1 for big documents. Where's the guidance on this?
pageTreeFromArray :: [PDFObject] -> PDFObject
pageTreeFromArray arr = 
    (PDFDict (fromList[
        ((PDFKey "Type"), (PDFSymbol "Pages")), 
        ((PDFKey "Count"), (PDFInt (length arr))),
        ((PDFKey "Kids"), (PDFArray arr))
    ]))
    
unParsePage :: PDFPageParsed -> PDFObject
unParsePage parsedPage = (PDFDict (fromList [
    ((PDFKey "Type"), PDFSymbol "Page"),
    ((PDFKey "Resources"), PDFDict (resources parsedPage)),
    ((PDFKey "Contents"), contents parsedPage),
    ((PDFKey "MediaBox"), boxToPDFObject (mediaBox parsedPage)),
    ((PDFKey "CropBox"),  boxToPDFObject (cropBox parsedPage))]))

boxToPDFObject :: PDFBox -> PDFObject
boxToPDFObject (Quad a b c d) = PDFArray [(PDFInt a),(PDFInt b),(PDFInt c),(PDFInt d)]
boxToPDFObject _ = PDFNull
    
-- the Unparsing functions. It's nice how short this is.
showPDFObject :: PDFObject -> String
showPDFObject (PDFString s) =  "(" ++ (escapeString s) ++ ")" 

showPDFObject (PDFSymbol s) =  "/" ++ s 

showPDFObject (PDFDict m) =
    "<<" ++ (foldrWithKey showKeyObject "" m) ++ " >>"

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
showPDFObject _ = error "can't showPDFObject a non PDFString object (yet)"

showKeyObject :: PDFKey -> PDFObject -> String -> String
showKeyObject (PDFKey key) obj initString = initString ++ 
    "/" ++ key ++ " " ++ (showPDFObject obj) ++ "\n"

showArrayObject :: String -> PDFObject -> String
showArrayObject initString obj = initString ++ 
    (showPDFObject obj) ++ " "

-- Components for creating a PDF document
fontDict :: String -> String -> PDFObject
fontDict name shortcut = 
    PDFDict (      -- ... this dict is the definition of it
        fromList [
            ((PDFKey "Type"), (PDFSymbol "Font")),
            ((PDFKey "Subtype"), (PDFSymbol "Type1")),
            ((PDFKey "BaseFont"), (PDFSymbol name)),
            ((PDFKey "Name"), (PDFSymbol shortcut)),
            ((PDFKey "Encoding"), (PDFSymbol "MacRomanEncoding"))
        ] )

-- add a mapping between a font shortcut and a font with a name, return new mapping 
addFontToDict :: String -> String -> PDFObject -> PDFObject
addFontToDict name shortcut oldDict = 
    addToDict oldDict shortcut (fontDict name shortcut)   
    -- someday silently don't add this if this shortcut's already in the dict
        
type PDF = State.State PDFState
data PDFState = 
    PDFState {
        -- masterDocument :: PDFObjectTreeFlattened,
        streamAccum   :: PDFObject, 
        rsrcDict      :: PDFObject, 
        fontsDict     :: PDFObject,
        pagesArray    :: [PDFPageParsed]
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
rundoc :: PDF () -> PDFDocumentParsed
rundoc m = d where
    myState = State.execState m newPDFState
    d = PDFDocumentParsed {
        pageList = pagesArray myState
        -- , globals = error "undefined globals"
    }

appendStream :: PDFObject -> String -> PDFObject
appendStream (PDFStream s) ns = PDFStream (s ++ ns)
appendStream _ _ = PDFError "Non-stream argument to appendStream"

newPDFState :: PDFState
newPDFState = PDFState {
        streamAccum = PDFStream "",
        rsrcDict = PDFDict (fromList []),
        fontsDict = PDFDict (fromList []),
        pagesArray = []
    }

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
    silly <- State.get
    putPDF silly

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

setFont :: String -> String -> Int -> PDF ()
setFont name shortcut fontSize = do
    myState <- State.get
    let s' = appendStream (streamAccum myState) ("/" ++ shortcut ++ " " ++ (show fontSize) ++ " Tf")
    let fonts' = addFontToDict name shortcut (fontsDict myState)
    let myState' = myState {
        streamAccum = s',
        fontsDict = fonts'
    }
    State.put (myState') 


globalProcSet :: PDFObject    
globalProcSet = PDFArray [(PDFSymbol "PDF"), (PDFSymbol "Text") ]

globalPageBox :: PDFBox
globalPageBox = Quad 0 0 300 300

-- The state monad is helping us thread the accumulated ObjectMap through the flattening process
-- without having to pass it all over the place.
type UnNest = State.State PDFObjectMap

traverseAndUnNest :: PDFObject -> UnNest PDFObject
traverseAndUnNest (PDFArray objs) = PDFArray `fmap` mapM (enPointerify PDFNull) objs
traverseAndUnNest (PDFDict myDict) = PDFDict `fmap` T.mapM (enPointerify PDFNull) myDict
traverseAndUnNest a = (enPointerify PDFNull) a

enPointerify :: PDFObject -> PDFObject -> UnNest PDFObject

enPointerify parent (PDFArray objs) = do
    objs' <- mapM (enPointerify parent) objs -- could also use T.mapM here
    case (length objs > 4) of
        True -> do
            reference (PDFArray objs')
        False -> return (PDFArray objs')

enPointerify parent node@(PDFDict objs) = do
    myReference <- reference PDFNull -- get a placeholder reference for my kids' "parent" reference
    objs'  <- T.mapM (enPointerify myReference) objs 
    let objs'' = case (isPageTreeNode node) of
                    True -> addParentPointer objs' parent
                    False -> objs'
                    -- False -> (Map.insert (PDFKey "NOTPARENT") (PDFString (show node)) objs')
    clobberReference (PDFDict objs'') myReference

-- ok, this is wack: if I don't "enpointerify" streams, it's not a valid PDF.
-- I'm having a hard time finding where this is stated in the spec. Sigh. that's
-- a day of my life I'd like back. :-/
enPointerify _parent str@(PDFStream _) = do
    reference str
    
enPointerify _parent o = return o

isPageTreeNode :: PDFObject -> Bool
isPageTreeNode (PDFDict dictMap) = case (Map.lookup (PDFKey "Type") dictMap) of 
        Just (PDFSymbol "Page")  -> True
        Just (PDFSymbol "Pages") -> True
        Just _                   -> False
        Nothing                  -> False
isPageTreeNode _ = False

addParentPointer :: PDFDictionaryMap -> PDFObject -> PDFDictionaryMap
addParentPointer inDict PDFNull = inDict
-- addParentPointer inDict PDFNull = (Map.insert (PDFKey "NULL PARENT") PDFNull inDict)
addParentPointer inDict parentPtr = (Map.insert (PDFKey "Parent") parentPtr inDict)


addParent :: PDFObject -> PDFObject -> PDFObject
addParent pd@(PDFDict _pd) parentRef = addToDict pd "Parent" parentRef

reference :: PDFObject -> UnNest PDFObject
reference obj = do
    dict <- State.get
    let (dict',ref) = addObjectGetRef dict obj
    State.put dict'
    return ref

clobberReference :: PDFObject -> PDFObject -> UnNest PDFObject
clobberReference object ref = do
    dict <- State.get
    State.put (clobberObjectWithRef dict object ref)
    return ref

addDocObjectGetRef :: PDFObject -> PDFObjectTreeFlattened -> (PDFObject, PDFObjectTreeFlattened)
addDocObjectGetRef obj (PDFObjectTreeFlattened root oldMap) = (objRef, (PDFObjectTreeFlattened root newMap)) where
    (newMap, objRef) = addObjectGetRef oldMap obj

-- this should be in my monad for recursivelyUnNest
addObjectGetRef :: PDFObjectMap -> PDFObject -> (PDFObjectMap, PDFObject)
addObjectGetRef om (PDFReference n g) = (om, (PDFReference n g)) -- don't create refs to refs
addObjectGetRef oldMap pdfobj = (newMap, newRef) where
    newRef = (PDFReference objNum 0)
    newMap = Map.insert objNum pdfobj oldMap
    objNum = (Map.size oldMap + 1)
    
clobberObjectWithRef :: PDFObjectMap -> PDFObject -> PDFObject -> PDFObjectMap
clobberObjectWithRef oldMap newObject (PDFReference n _) = Map.insert n newObject oldMap
clobberObjectWithRef _ _ _ = error ("internal error: bad args to clobberObjectWithRef")

enpointerifyRoot :: PDFObjectTreeFlattened -> PDFObjectTreeFlattened
enpointerifyRoot (PDFObjectTreeFlattened rootDict oldMap) = (PDFObjectTreeFlattened dictRef newMap) where
    (newMap, dictRef) = addObjectGetRef oldMap rootDict
    
data ObjectIndices = ObjectIndices [Int] deriving (Show)

-- empties the PDFObjectList, printing each object as we go, adding
-- its offset in the output file to "ObjectIndices"
printFlatTree' :: Handle -> PDFObjectTreeFlattened -> ObjectIndices -> Int -> Int -> IO PDFObjectTreeFlattened
printFlatTree' 
        h
        (PDFObjectTreeFlattened _a objectMap )
        (ObjectIndices ixs)  
        currIx
        objNum = case (mapSize >= objNum) of
            True -> do
                hPutStr h (prefixStr)
                hPutStr h (str)
                hPutStr h (postFixStr)
                printFlatTree' h (PDFObjectTreeFlattened _a objectMap ) (ObjectIndices (ixs ++ [currIx])) newIx (objNum + 1)
            False -> printFlatTree'' h (PDFObjectTreeFlattened _a objectMap ) (ObjectIndices ixs) currIx
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
printFlatTree'' :: Handle -> PDFObjectTreeFlattened -> ObjectIndices -> Int -> IO PDFObjectTreeFlattened
printFlatTree''
        h
        inDoc@(PDFObjectTreeFlattened rootRef _ )
        (ObjectIndices (ixs)) 
        currIx = do
            printXRefIndexes h ixs 0
            let numObjects = 1 + length ixs
            printTrailer h rootRef numObjects currIx 
            return inDoc 

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




