--------------------------------------------------------------------
-- |
-- Module      : Text.PDF.Types
-- Description : Representing PDF content
-- Copyright   : (c) Dylan McNamee, 2008, 2009
-- License     : BSD3
--
-- Maintainer: Dylan McNamee <dylan@galois.com>
-- Stability : provisional
-- Portability: portable
--
-- Representing PDF content
--------------------------------------------------------------------
module Text.PDF.Types where

import Data.Map as Map

newtype PDFKey = PDFKey String deriving (Show, Ord, Eq)

type PDFDictionaryMap = Map PDFKey PDFObject
type PDFObjectMap = Map Int PDFObject
type PDFPageList = [PDFPageParsed] 

type PDFDocumentExploded = PDFObject

-- this is the pure PDFObject representation of a document. It's useful for generation,
-- (all you need to do is pretty-print the structure) but not very useful for 
-- manipulation, because you have much more to parse. 
data PDFDocument = 
    PDFDocument {
        catalogDict :: PDFObject,    -- the catalog dictionary (the root of the Page Tree & friends)
        objectList  :: PDFObjectMap  -- a map from objectNum -> PDFObject
    } deriving (Show)

-- the following types hint at PDF parsing functions to come.
-- I've got them written, but they're a mess, so I'm leaving them 
-- out for the initial submission.  It could be that these data types
-- were the wrong first step, but we'll cross that bridge as I clean things up.

data PDFDocumentParsed = 
    PDFDocumentParsed {
        pageList :: PDFPageList, -- later PDFPageList
        globals  :: PDFGlobals
    } deriving (Show)

data PDFGlobals = 
    PDFGlobals {
        -- global page dictionary definitions, if there are any
    } deriving (Show)

data PDFPageParsed = 
    PDFPageParsed {
        resources :: PDFDictionaryMap,
        fonts     :: PDFDictionaryMap,
        contents  :: PDFObject,
        mediaBox  :: PDFBox,
        cropBox   :: PDFBox -- other boxes too? 
    } deriving (Show)

data PDFFontParsed = 
    PDFFontParsed {
        baseFont  :: String,
        otherElts :: PDFDictionaryMap
    } deriving (Show)
    
emptyPage :: PDFPageParsed
emptyPage = PDFPageParsed {
    fonts = Map.empty,
    resources = Map.empty,
    contents = emptyStream
}

-- bounding boxes are in absolute point coordinates (llx, lly, urx, ury)
data PDFBox = 
    Quad Int Int Int Int |
    NullBox
    deriving (Show,Ord,Eq)

emptyStream :: PDFObject
emptyStream = PDFStream ""

data PDFObject = 
    PDFString String |
    PDFSymbol String |
    PDFStream String |
    PDFStreamDict String PDFObject | 
    PDFInt Int |
    PDFFloat Float |
    PDFReference Int Int |
    PDFArray [PDFObject] |
    PDFXObject String | -- should crack this nut sometime...svg's, jpeg's, .mov's
    PDFDict PDFDictionaryMap |
    PDFComment String |
    PDFError String |
    PDFPageRaw PDFObject |
    PDFNull 
    deriving (Show,Ord,Eq)

