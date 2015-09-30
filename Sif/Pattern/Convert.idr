module Sif.Pattern.Convert

import Data.GraphViz.SimpleDot

import Edda
import Edda.Writer.Org
import Edda.Writer.LaTeX
import Edda.Writer.CommonMark

import XML.DOM

import Freyja
import Freyja.Convert.XML

import Sif.Types
import Sif.Pattern.Model
import Sif.Pattern.API
--import Sif.Pattern.Convert.XML
import Sif.Pattern.Convert.Edda
import Sif.Pattern.Convert.String
import Sif.Pattern.Convert.Freyja

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
convTy FREYJA  = PatternDoc

covering
convTo : (fmt : SifOutFormat) -> PATTERN impl d -> Maybe (convTy fmt)
convTo FREYJA  p = Just $ toFreyja p
convTo XML     p = Just $ toXML (toFreyja p)

convTo EDDA    p = Just $ toEdda p
convTo ORG     p = Just $ org $ toEdda p
convTo LATEX   p = Just $ latex $ toEdda p
convTo CMARK   p = Just $ markdown $ toEdda p

convTo COMPACT p = Just $ String.toString p
convTo STRING  p = Just $ String.toString p

convTo DOT     p = Nothing -- Just $ toDot p

convTo IDRIS   p = Nothing


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
