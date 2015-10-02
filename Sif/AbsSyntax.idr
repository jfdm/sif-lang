-- ----------------------------------------------------------- [ AbsSyntax.idr ]
-- Module    : AbsSyntax.idr
-- Copyright : (c) Jan de Muijnck-Hughes
-- License   : see LICENSE
-- --------------------------------------------------------------------- [ EOH ]
module Sif.AbsSyntax

import GRL.Lang.GLang
import Sif.Types
import Sif.Pattern.Model

namespace AST

  data SifAST : SifTy -> Type where
    Req : (ident : String)
       -> (ty    : RTy)
       -> (title : String)
       -> (desc  : Maybe String)
       -> SifAST TyREQ

    Problem : (ident : String)
           -> (title : String)
           -> (desc  : Maybe String)
           -> (context : Pair String SifDomain)
           -> List (SifAST TyREQ)
           -> SifAST TyPROBLEM

    Affect : (value : CValue)
          -> (id : String)
          -> (desc : Maybe String)
          -> SifAST TyAFFECTS

    Trait : (ty : TTy)
         -> (title : String)
         -> (value : SValue)
         -> (desc  : Maybe String)
         -> (affects : List (SifAST TyAFFECTS))
         -> SifAST tyTRAIT

    Property : (title : String)
            -> (desc : Maybe String)
            -> (traits : List (SifAST TyTRAIT))
            -> SifAST tyPROPERTY

    Solution : (title : String)
            -> (probID : Pair String (Maybe String))
            -> (desc : Maybe String)
            -> (ctxtID : String)
            -> (properties : List (SifAST TyPROPERTY))
            -> SifAST TySOLUTION

    Pattern : (title : String)
           -> (desc : Maybe String)
           -> (problem : SifAST TyPROBLEM)
           -> (solution : SifAST TySOLUTION)
           -> SifAST TyPATTERN


  instance Show (SifAST ty) where
    show (Req i ty t d)       = unwords ["Req", show i, show ty, show t, show d]
    show (Problem i t d c rs) = unwords ["Problem", show i, show t, show d, show c, show rs]
    show (Affect v i d)       = unwords ["Affect", show v, show i, show d]
    show (Trait ty t v d as)  = unwords ["Trait", show ty, show t, show v, show d, show as]
    show (Property t d ts)    = unwords ["Property", show t, show d, show ts]
    show (Solution t p d c ps) = unwords ["Solution", show t, show p, show d, show c, show ps]
    show (Pattern t d p s)     = unwords ["Pattern", show t, show d, show p, show s]

-- --------------------------------------------------------------------- [ EOF ]
