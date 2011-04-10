--------------------------------------------------------------------
-- |
-- Module      : Text.PDF.Utils
-- Description : Functions for manipulating PDF content.
-- Copyright   : (c) Dylan McNamee, 2008, 2009
-- License     : BSD3
--
-- Maintainer: Dylan McNamee <dylan@galois.com>
-- Stability : provisional
-- Portability: portable
--
-- Convenience functions for manipulating and constructing
-- PDF fragments in Haskell.
--------------------------------------------------------------------
module Text.PDF.Utils where

import Data.Map as Map
import Data.Array as Array
import Data.Maybe 

import Text.PDF.Types

traversePDFReference :: PDFObject -> PDFDocument -> PDFObject
traversePDFReference (PDFReference objNum _g) (PDFDocument _ objMap ) = 
    fromMaybe (PDFError ("Unable to find object " ++ (show objNum))) (Map.lookup objNum objMap)
traversePDFReference _ _ = error "Bad arguments to traversePDFReference"
    
-- I decided to make all nodes in the tree be created with this function,
-- toggled by whether parent is PDFNull or not.

-- create the root node: it has no parent 
-- pageTreeNode pagesArray parent -> PDFDict
pageTreeNode :: PDFObject -> PDFObject -> PDFObject     
pageTreeNode pageTreeRef PDFNull = PDFDict (fromList [
            ((PDFKey "Type"), (PDFSymbol "Catalog")), 
            ((PDFKey "Pages"), pageTreeRef)])

-- this is all internal nodes, which have parents
pageTreeNode pagesArrayRef parentRef = PDFDict (fromList [
            ((PDFKey "Type"), (PDFSymbol "Catalog")), 
            ((PDFKey "Pages"), pagesArrayRef),
            ((PDFKey "Parent"), parentRef)])

addObjectGetRef :: PDFObject -> PDFDocument -> (PDFObject, PDFDocument)
addObjectGetRef (PDFReference n g) d = ((PDFReference n g), d)
addObjectGetRef pdfobj (PDFDocument root oldMap ) = (newRef, newDoc) where
    newRef = (PDFReference objNum 0)
    newDoc = (PDFDocument root newMap )
    newMap = Map.insert objNum pdfobj oldMap
    objNum = (Map.size oldMap + 1)

joinPDFDocument :: PDFDocument -> PDFDocument -> PDFDocument
joinPDFDocument = undefined

filterPages :: (PDFObject -> Bool) -> PDFDocument -> PDFDocument
filterPages = undefined

{-
applyPageTransformer :: (PDFPageParsed -> PDFPageParsed) -> PDFDocumentParsed -> PDFDocumentParsed
applyPageTransformer transformer inDoc = inDoc {
        pageList = Prelude.map transformer (pageList inDoc) 
    }

rotateDocument :: PDFDocumentParsed -> Float -> PDFDocumentParsed
rotateDocument doc degrees = applyPageTransformer (rotatePage degrees doc) doc

rotatePage :: Float -> PDFDocumentParsed -> PDFPageParsed -> PDFPageParsed
rotatePage degrees inDoc inPage = inPage {
        contents = newContents
    } where
        (_pageWidth, pageHeight) = getDocDimensions inDoc
        newContents = case (contents inPage) of 
            PDFArray elts -> doRotate degrees (PDFArray elts) pageHeight
            str@PDFStream{} -> doRotate degrees (PDFArray [str]) pageHeight
            PDFNull -> PDFNull
            o -> error ("bad contents type found in rotate page" ++ (show o))
-}

-- this function is asking to be higher-order, instead of string manipulating
doRotate :: Float -> PDFObject -> Float -> PDFObject
doRotate degrees (PDFArray elts) pageHeight = 
    PDFArray ((PDFStream (" q " ++ 
                (rotateString degrees) ++ 
                (translateString pageHeight 0) )) : 
            elts ++ [(PDFStream " Q ")]) where

doRotate _ _ _ = error "non-array type to doRotate"
        
getDocDimensions :: PDFDocumentParsed -> (Float, Float)
getDocDimensions _ = (690.0, 720.0) 

rotateString :: Float -> String
rotateString a = " " ++ (show cosA) ++ " " ++ (show sinA) ++ " -" ++ (show sinA) ++
    " " ++ (show cosA) ++ " cm " where
    cosA = cos a
    sinA = sin a

translateString :: Float -> Float -> String
translateString tx ty = " 1 0 0 1 " ++ (show tx) ++ " " ++ (show ty) ++ " cm "

startsWith :: String -> String -> Bool 
startsWith s1 s2 = (take (length s2) s1) == s2

arrayToList :: (Int, Int) -> Array Int a -> [a] 
arrayToList (offset, len) ar  
                | len <= 0 = []
                | (len == 1) = [ar Array.! offset]
                | True = (ar Array.! offset) : (arrayToList (offset + 1, len - 1) ar)

skipLine :: String -> String
skipLine [] = []
skipLine ('\n' : cs) = cs
skipLine (_ : cs) = skipLine cs

dictLookup :: String -> PDFObject -> String -> PDFObject
dictLookup key (PDFDict dictMap) errorMsg =
    fromMaybe (PDFError errorMsg) (Map.lookup (PDFKey key) dictMap)
dictLookup _ _ _ = undefined "bad args to dictLookup"

addToDict :: PDFObject -> String -> PDFObject -> PDFObject
addToDict (PDFDict dictMap) key value = PDFDict (Map.insert (PDFKey key) value dictMap)
addToDict _ _ _ = PDFError "bad args to addToDict"

intValue :: PDFObject -> Maybe Int
intValue (PDFInt n) = Just n
intValue _ = Nothing
    
escapeString :: String -> String
escapeString [] = []
escapeString (c:cs) = case c of 
    '(' -> ('\\' : (c : escapeString cs))
    ')' -> ('\\' : (c : escapeString cs))
    '\\' -> ('\\' : (c : escapeString cs))
    _ -> (c : escapeString cs)

padTo, padTo' :: String -> Int -> String
padTo a n = padTo' a (n - (length a))

padTo' a 0 = a
padTo' a n = ('0' : padTo' a (n - 1)) -- todo: catch the case n < 0
