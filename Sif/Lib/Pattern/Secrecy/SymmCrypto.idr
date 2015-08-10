-- ---------------------------------------------------------- [ SymmCrypto.idr ]
-- Module    : SymmCrypto.idr
-- Copyright : (c) Jan de Muijnck-Hughes
-- License   : see LICENSE
-- --------------------------------------------------------------------- [ EOH ]

||| Modelling Information Secrecy using Symmetric Cryptography.
module Sif.Lib.Pattern.Secrecy.SymCrypto

import Sif.Pattern

import Sif.Lib.Problem.InformationSecrecy

import GRL.Eval

-- ---------------------------------------------- [ Asymmetric Crypto Solution ]

singleKey : PROPERTY
singleKey = mkProperty "Single Key" Nothing [t1,t2]
  where
    t1 : DISADVANTAGE
    t1 = mkDisadvantage "Use of a single key" Nothing SATISFIED
            [mkLink BREAK recipient_confidentiality]

    t2 : DISADVANTAGE
    t2 = mkDisadvantage "Key distribution" Nothing SATISFIED
            [mkLink HURTS ttl_security]

maths : PROPERTY
maths = mkProperty "Maths Algorithm" Nothing [t1]
  where
    t1 : ADVANTAGE
    t1 = mkAdvantage "Speed" Nothing SATISFIED
            [mkLink MAKES mech_perf]

symCrypto : SOLUTION
symCrypto = mkSolution "Symmetric Crypto" Nothing
                [singleKey, maths]

infoSecSymmCrypto : PATTERN
infoSecSymmCrypto = mkPattern
    "Information Security using Symmetric Crypto"
    Nothing
    infosec
    symCrypto

-- --------------------------------------------------------------------- [ EOF ]
