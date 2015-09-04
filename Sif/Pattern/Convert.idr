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
convTy ORG     = String
convTy XML     = Document DOCUMENT
convTy DOT     = SimpleDot GRAPH
convTy EDDA    = Edda PRIME MODEL
convTy COMPACT = String
convTy IDRIS   = String
convTy STRING  = String

convTo : (fmt : SifOutFormat) -> PATTERN impl -> Maybe (convTy fmt)
convTo XML     p = Just $ toXML p

convTo EDDA    p = Just $ toEdda p
convTo ORG     p = Just $ org $ toEdda p

convTo DOT     p = Nothing -- Just $ toDot p
convTo COMPACT p = Just $ toString p
convTo IDRIS   p = Nothing
convTo STRING  p = Just $ toString p


||| o'rrible code
covering
showConvPattern : SifOutFormat -> PATTERN impl ->  Maybe String
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
    Just r  => Just (show r)

showConvPattern IDRIS p =
  case (the (Maybe (convTy IDRIS)) (convTo IDRIS p)) of
    Nothing => Nothing
    Just r  => Just (show r)

showConvPattern EDDA p =
  case (the (Maybe (convTy EDDA)) (convTo EDDA p)) of
    Nothing => Nothing
    Just r  => Just (show r)

showConvPattern COMPACT p =
  case (the (Maybe (convTy COMPACT)) (convTo COMPACT p)) of
    Nothing => Nothing
    Just r  => Just r
showConvPattern STRING p =
  case (the (Maybe (convTy STRING)) (convTo STRING p)) of
    Nothing => Nothing
    Just r  => Just r

showConvPattern _ _ = Nothing
