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


-- --------------------------------------------------------------------- [ EOF ]
