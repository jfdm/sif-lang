module Sif.Pattern.Convert

import Data.GraphViz.SimpleDot

import Edda
import Edda.Writer.Org
import Edda.Writer.LaTeX
import Edda.Writer.CommonMark

import XML.DOM

import Sif.Types
import Sif.Pattern.Model
import Sif.Pattern.API
import Sif.Pattern.Convert.XML
import Sif.Pattern.Convert.Edda
import Sif.Pattern.Convert.String

%default partial

covering
convTy : SifOutFormat -> Type
convTy LATEX   = String -- This should be a data structure...
convTy CMARK   = String -- This should be a data structure...
convTy ORG     = String -- This should be a data structure...
convTy XML     = Document DOCUMENT
convTy DOT     = SimpleDot GRAPH
convTy EDDA    = Edda PRIME MODEL
convTy COMPACT = String
convTy IDRIS   = String
convTy STRING  = String

covering
convTo : (fmt : SifOutFormat) -> PATTERN impl d -> Maybe (convTy fmt)
convTo XML     p = Just $ toXML p

convTo EDDA    p = Just $ toEdda p
convTo ORG     p = Just $ org $ toEdda p
convTo LATEX   p = Just $ latex $ toEdda p
convTo CMARK   p = Just $ markdown $ toEdda p

convTo DOT     p = Nothing -- Just $ toDot p
convTo COMPACT p = Just $ String.toString p
convTo IDRIS   p = Nothing
convTo STRING  p = Just $ String.toString p


||| o'rrible code
covering
showConvPattern : SifOutFormat -> PATTERN impl d ->  Maybe String
showConvPattern DOT p =
  case (the (Maybe (convTy DOT)) (convTo DOT p)) of
    Nothing => Nothing
    Just r  => Just (show r)

showConvPattern XML p =
  case (the (Maybe (convTy XML)) (convTo XML p)) of
    Nothing => Nothing
    Just r  => Just (show @{xml} r)

showConvPattern ORG p =
  case (the (Maybe (convTy ORG)) (convTo ORG p)) of
    Nothing => Nothing
    Just r  => Just r

showConvPattern CMARK p =
  case (the (Maybe (convTy CMARK)) (convTo CMARK p)) of
    Nothing => Nothing
    Just r  => Just r

showConvPattern LATEX p =
  case (the (Maybe (convTy LATEX)) (convTo LATEX p)) of
    Nothing => Nothing
    Just r  => Just r

showConvPattern IDRIS p =
  case (the (Maybe (convTy IDRIS)) (convTo IDRIS p)) of
    Nothing => Nothing
    Just r  => Just (show r)

showConvPattern EDDA p =
  case (the (Maybe (convTy EDDA)) (convTo EDDA p)) of
    Nothing => Nothing
    Just r  => Nothing -- TODO Just (show r)

showConvPattern COMPACT p =
  case (the (Maybe (convTy COMPACT)) (convTo COMPACT p)) of
    Nothing => Nothing
    Just r  => Just r

showConvPattern STRING p =
  case (the (Maybe (convTy STRING)) (convTo STRING p)) of
    Nothing => Nothing
    Just r  => Just r

showConvPattern _ _ = Nothing

-- --------------------------------------------------------------------- [ EOF ]
