--------------------------------------------------------------------
-- |
-- Module      : Text.PDF.Types
-- Description : Representing PDF content
-- Copyright   : (c) Dylan McNamee, 2008
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
type PDFObjectList = [PDFObject]
type PDFPageList = [PDFPageParsed] 

data PDFGlobals = 
    PDFGlobals {
        -- more stuff goes here - especially global page dictionary definitions
        rootObject :: PDFObject
    } deriving (Show)

data PDFDocument = 
    PDFDocument {
        catalogDict :: PDFObject,    -- the catalog dictionary
        objectList :: PDFObjectList  -- logically, a map from objectNum -> PDFObject
    } deriving (Show)

data PDFDocumentParsed = 
    PDFDocumentParsed {
        pageList :: PDFPageList,
        globals  :: PDFGlobals
    } deriving (Show)

data PDFPageParsed = 
    PDFPageParsed {
        fonts     :: PDFDictionaryMap,
        resources :: PDFDictionaryMap,
        contents  :: PDFObject, 
        parent    :: PDFObject -- a reference   -- other stuff too
    } deriving (Show)

emptyPage = PDFPageParsed {
    fonts = Map.empty,
    resources = Map.empty,
    contents = nullStream,
    parent = PDFNull
}

nullStream = PDFStream ""

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

