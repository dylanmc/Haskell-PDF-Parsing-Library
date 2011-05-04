--------------------------------------------------------------------
-- |
-- Module      : Text.PDF.Transformations
-- Description : Functions for transforming PDF content.
-- Copyright   : (c) Dylan McNamee 2011
-- License     : BSD3
--
-- Maintainer: Dylan McNamee <dylan@galois.com>
-- Stability : provisional
-- Portability: portable
--
--------------------------------------------------------------------
module Text.PDF.Transformations where

import Text.PDF.Types

-- Overlay a string each page of a document
watermarkDocument :: PDFDocumentParsed -> String -> PDFDocumentParsed
watermarkDocument doc@(PDFDocumentParsed pages {- _dict -} ) markString = doc {
        pageList = Prelude.map (appendStreamToPage markString) pages
        -- , globals = dict
    }

appendStreamToPage :: String -> PDFPageParsed -> PDFPageParsed
appendStreamToPage string page = page {
        contents = case (contents page) of
            (PDFStream s) -> PDFArray [(PDFStream s), (PDFStream string)]
            (PDFArray streams) -> PDFArray (streams ++ [(PDFStream string)] )
            _ -> error "appendStreamToPage: neither Stream nor Array in contents of a page"
    }
