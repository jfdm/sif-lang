-- | Sif Keywords
module Sif.Keywords where

import Data.List

-- | Operators
sifOperators = [sifOpAssignment, sifOpAggregation, sifOpAssociation,
                sifOpSpecialisation, sifOpRealisation, sifOpDescription] 
sifOpAssignment     = ":="
sifOpAggregation    = "o-"
sifOpAssociation    = "->"
sifOpSpecialisation = "<-"
sifOpRealisation    = "=>"
sifOpDescription    = ":"

-- | Keywords
sifKeywords = concat [sifKWordRelations, sifKWordTypeModifiers, sifKWordTypes,
                      sifKWordMisc]      

-- --------------------------------------------------------------- [ Relations ]
sifKWordRelations = [sifKWordAssociation, sifKWordAggregation,
                     sifKWordSpecialisation, sifKWordRealisation]
sifKWordAssociation    = "linkedTo"
sifKWordAggregation    = "uses"
sifKWordSpecialisation = "extends"
sifKWordRealisation    = "implements"

-- ---------------------------------------------------------- [ Type Modifiers ]
sifKWordTypeModifiers = [sifKWordTypModAbs, sifKWordTypModConc]
sifKWordTypModAbs  = "Abstract"
sifKWordTypModConc = "Concrete"

-- ------------------------------------------------------------------- [ Types ]
sifKWordTypes     = [sifKWordTypPat, sifKWordTypComp, sifKWordTypSys,
                     sifKWordTypDeplo, sifKWordTypAdmin, sifKWordTypImpl] 
sifKWordTypPat    = "Pattern"
sifKWordTypComp   = "Component"
sifKWordTypSys    = "System"
sifKWordTypDeplo  = "Deployment"
sifKWordTypAdmin  = "Admin"
sifKWordTypImpl   = "Implementation"

-- -------------------------------------------------------------------- [ Misc ]
sifKWordMisc = [sifKWordFrom, sifKWordImport, sifKWordRelation,
                sifKWordPattern, sifKWordLang, sifKWordAs]
sifKWordFrom     = "from"
sifKWordAs       = "as"
sifKWordImport   = "import"
sifKWordRelation = "relations"
sifKWordPattern  = "patterns"
sifKWordLang     = "language"

-- | Misc

sifCmtLine = "--"

-- --------------------------------------------------------------------- [ EOF ]
