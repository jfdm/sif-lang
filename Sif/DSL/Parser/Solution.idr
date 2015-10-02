-- ------------------------------------------------------------ [ Solution.idr ]
-- Module    : Solution.idr
-- Copyright : (c) Jan de Muijnck-Hughes
-- License   : see LICENSE
-- --------------------------------------------------------------------- [ EOH ]
||| Parser problem specifications
module Sif.DSL.Parser.Solution

-- ----------------------------------------------------------------- [ Imports ]
import Lightyear
import Lightyear.Char
import Lightyear.Strings

import GRL.Lang.GLang

import Sif.Types
import Sif.AbsSyntax

import Sif.DSL.Parser.Utils
import Sif.DSL.Parser.Common
import Sif.DSL.Parser.Problem

import Test.Parsing

-- -------------------------------------------------------------- [ Directives ]
%default partial
%access private

-- ----------------------------------------------------------------- [ Parsers ]

traitTy : Parser TTy
traitTy = (keyword "Advantage"    *> return ADV)
      <|> (keyword "Disadvantage" *> return DIS)
      <|> (keyword "Trait"        *> return GEN)
      <?> "Trait Type"

cValue : Parser CValue
cValue = (keyword "Makes"   *> return MAKES)
     <|> (keyword "Helps"   *> return HELPS)
     <|> (keyword "SomePos" *> return SOMEPOS)
     <|> (keyword "Unknown" *> return UNKNOWN)
     <|> (keyword "SomeNeg" *> return SOMENEG)
     <|> (keyword "Breaks"  *> return BREAK)
     <|> (keyword "Hurts"   *> return HURTS)
     <?> "CValue"

sValue : Parser SValue
sValue = (keyword "Denied"    *> return DENIED)
     <|> (keyword "WeakDen"   *> return WEAKDEN)
     <|> (keyword "WeakSatis" *> return WEAKSATIS)
     <|> (keyword "Satisfied" *> return SATISFIED)
     <|> (keyword "Undecided" *> return UNDECIDED)
     <|> (keyword "None"      *> return NONE)
     <?> "Trait Type"

affect : Parser $ SifAST TyAFFECTS
affect = do
    c <- cValue
    spaces
    i <- ident
    spaces
    d <- opt $ (keyword "by" *> descString <* spaces)
    pure $ AST.Affect c i d
  <?> "Affects"

affects : Parser $ List $ SifAST TyAFFECTS
affects = do
    keyword "Affects"
    as <- braces $ commaSep1 affect
    sifComment
    pure as

trait : Parser $ SifAST TyTRAIT
trait = do
      d <- opt sifDoc
      ty <- traitTy
      t <- title
      keyword "is"
      s <- sValue
      as <- braces affects
      sifComment
      pure $ AST.Trait ty t s d as
    <?> "Trait"

property : Parser $ SifAST TyPROPERTY
property = do
      d <- opt sifDoc
      keyword "Property"
      t <- title
      ts <- braces $ some trait
      sifComment
      pure $ AST.Property t d ts
    <?> "Property"

doctype : Parser ()
doctype = do
      sifComment
      keyword "sif"
      spaces
      string "solution"
      sifComment
  <?> "DocType"

solHeader : Parser (Maybe String, String, String, String)
solHeader = do
      d <- opt sifDoc
      keyword "Solution"
      t <- title
      keyword "solves"
      spaces
      pID <- ident
      keyword "in"
      cID <- ident
      pure (d, t, pID, cID)
  <?> "Solution Header"

public
solution : Parser $ (SifAST TySOLUTION)
solution = do
      doctype
      pd <- opt desc
      (d, t, pID, cID) <- solHeader
      ps <- braces $ some property
      pure $ AST.Solution t (pID,pd) d cID ps
    <?> "Solution"

public
runTests : IO ()
runTests = do
  putStrLn $ heading "Parsing Solution Tests"
  canParse (Just "Solution Header") solHeader "Solution \"Why does this not work\" solves adt in blah"

  canParse (Just "Solution Header with Docs")
           solHeader
           """> I am docs
> I am docs

Solution "Why does this not work" solves adt in blah"""


  canParse (Just "Affect") affect "Hurts foobar"
  canParse (Just "Affect") affect "Hurts foobar by \"doing something\""

  canParse (Just "Affects")
           (many affects)
           """Affects { Makes foobar }

Affects {Makes foobar, Breaks bank by "dropping like its hot"}
"""
  canParse (Just "Affect with Docs")
           (many affects)
           """> I am doc
Affects { Makes foobar }

> AASAS
Affects {Makes foobar, Breaks bank by "dropping like its hot"}
"""

  canParse (Just "Trait")
            (trait)
            """Trait "A Trait" is Satisfied {
  Affects {Makes foobar, Breaks bank by "dropping like its hot"}}
"""

  canParse (Just "Trait with Docs")
            (trait)
            """> Asasasasas
Trait "A Trait" is Satisfied {
  Affects {Makes foobar, Breaks bank by "dropping like its hot"}}
"""

  canParse (Just "Property")
            (property)
            """Property "A Property" { Trait "A Trait" is Satisfied {
  Affects {Makes foobar, Breaks bank by "dropping like its hot"}}}
"""

  canParse (Just "Property with Docs")
            (property)
            """> I am docs

Property "A Property" { Trait "A Trait" is Satisfied {
  Affects {Makes foobar, Breaks bank by "dropping like its hot"}}}
"""

  canParse (Just "A Trait")
           trait
           """> There is possibility that the implementation of the algorithm is insecure/poor.
	Trait "Implementation InSecure" is WeakSatis {
	  Affects {
	    SomeNeg recipConf, SomeNeg dataConf, Unknown minDisrupt, SomeNeg secureImpl
	  }
	}
           """

  canParse (Just "Another Heading")
           solHeader
           """> Public key crypto schemes are an application of asymmetric
> cryptography in which the public/private key pair is used for
> encryption/decryption respectively. Asymmetric crypto schemes
> consist of three algorithms.
> \
> + keygen :: Given a security parameter generates a public private key pair.
> + encrypt :: Given a public key and a message, encrypts the message under the given public key.
> + decrypt :: Given a private key and a ciphertext, =decrypt= returns a readable plaintext if the private key was paired with the ciphertexts encryption key.
Solution "Public Key Cryptography" solves infosec in std
           """

  canParse (Just "Maths Algorithm")
           property
           """> The underlying mathematical algorithm that provides the asymmetric cipher used.
  Property "Maths Algorithm" {

    > There will be a formal proof of the operations working.
    Advantage "Formal Guarantee" is Satisfied {
      Affects { Helps dataConf, Helps recipConf }
    }

	> The maths algorithm is generally computationally expensive on large data.
    Disadvantage "Computationally Expensive" is WeakSatis {
      Affects { Hurts mechPerf, Hurts minDisrupt }
    }

  }
"""
  canParse (Just "Implementation in Code")
           property
           """> The implementation in Code
  Property "Implementation in Code" {

    > There is possibility that the implementation of the algorithm is insecure/poor.
	Trait "Implementation InSecure" is WeakSatis {
	  Affects {
	    SomeNeg recipConf, SomeNeg dataConf, Unknown minDisrupt, SomeNeg secureImpl
	  }
	}

	> Some implementations really make it hard to understand things
	Trait "Implementation obfuscates understanding" is WeakDen {
	  Affects { Hurts understandable, Hurts minDisrupt }
    }
  }
"""

  canParse (Just "Ket Distribution Trait")
           trait
           """> A means of key distribution is required to ensure that public key ownership can be attested.
	Trait "Key Distribution" is Satisfied {
       Affects {
         SomeNeg recipConf by "Keys are distributed insecurely.",
		 SomeNeg minDisrupt by "Incorrect Distribution."
       }
    }"""

  canParse (Just "Key Pairs")
           property
           """> There are two keys in use.
  Property "Key Pairs" {

    > Reduces problems with Key Security
    Advantage "PublicEnc/PrivateDec Keys" is Satisfied {
       Affects { Makes recipConf by "Only recipient has key to unlock the message" }
    }

	> A means of key distribution is required to ensure that public key ownership can be attested.
	Trait "Key Distribution" is Satisfied {
       Affects {
         SomeNeg recipConf by "Keys are distributed insecurely.",
		 SomeNeg minDisrupt by "Incorrect Distribution."
       }
    }

	> This solution requires the management of keys. That is, for each recipient the sender has to obtain a encryption key.
    Trait "Key Management" is Satisfied {
	   Affects {
	     SomeNeg dataConf by "Possibly sending data encrypted under the wrong key."
	   }
	}
  }
"""

  canParse (Just "Var Sec Params")
           property
           """> Different algorithm's will have different security parameters, and may allow different key lengths.
  Property "Variable Security Parameters" {

    > Many algorithms have variable security levels. Data can been secured at various levels.
    Advantage "Greater Security" is Satisfied {
	    Affects { Helps multiSecLevels }
	}

   > Common parameters are required to be selected and agreed upon.
   Trait "Parameter Selection" is Satisfied {
        Affects {
		    SomeNeg multiSecLevels by "insecure parameters being selected."
		  , SomeNeg secureImpl     by "insecure paremeters being selected."
		}
   }

   > Some parameters though secure might occur a greater processing cost.
   Disadvantage "Parameter Computation" is Satisfied {
       Affects { Hurts mechPerf, Hurts minDisrupt }
   }
  }"""

-- --------------------------------------------------------------------- [ EOF ]
