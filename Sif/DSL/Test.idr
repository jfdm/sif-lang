-- --------------------------------------------------------------- [ Tests.idr ]
-- Module    : Tests.idr
-- Copyright : (c) Jan de Muijnck-Hughes
-- License   : see LICENSE
-- --------------------------------------------------------------------- [ EOH ]
module Sif.DSL.Test

import Test.Parsing

import Lightyear
import Lightyear.Char
import Lightyear.Strings

import Sif.Types
import Sif.AbsSyntax
import Sif.DSL.Parser.Problem
import Sif.DSL.Parser.Solution

export
runTests : IO ()
runTests = do
  putStrLn $ heading "Complete File Tests"
  canParse (Just "Problem")
           problem
           """sif problem

> Given two entities Alice and Bob, how can Bob give a message to
> Alice such that only the intended recipient, Alice, can be guaranteed
> to access the message.

infosec <- Problem "Information Secrecy"

> Core principle that ensures that the data is kept confidential.
>
> This functional requirement represents the core principle behind
> data confidentiality, the data should be kept confidential. The
> attacker may obtain knowledge about the sender/recipient, means and
> times of transport but the information contained inside should not be
> accessible.

dataConf <- Functional "Data Confidentiality"

> The data should be viewable only by the intended recipient.
>
> A related functional requirement is that the recipient of the
> message should be the only entity capable of accessing the data
> contained within the message.

recipConf <- Functional "Recipient Confidentiality"

> This performance requirement represents the notion that not all data
> needs equivalent levels of protection.  Data only needs to be
> protected for the lifetime that the confidentiality needs to be
> guaranteed.

multiSecLevels <- Performance "Suitable Security Level"

> This usability requirement represents the idea that the actual act
> of securing data should not hinder upon the usage of any proposed
> mechanism. A user of the solution must not be inconvenienced by its
> use.

mechPerf <- Performance "Suitable performance"

> A secondary usability requirement targets the level of knowledge
> that an end-user of the solution must possess to use the solution.
> The user should need minimal knowledge of the underlying system.

understandable <- Usability "Comprehensible by Non-Experts"

> The resulting solution should have minimal impact on the workflow of
> the user.

minDisrupt <- Usability "Minimal Workflow Disruption"

> The implementation should be implemented securely.

secureImpl <- Reliability "Secure Implementation"
"""

  canParse (Just "Solution Complete")
           solution
           """sif solution

Description "Information Secrecy is about keeping data confidential. Public key encryption provides a mathematically guaranteed mechanism that allows data to be obfuscated such that only the recipient can reconstitute the messages original meaning."

> Public key crypto schemes are an application of asymmetric
> cryptography in which the public/private key pair is used for
> encryption/decryption respectively. Asymmetric crypto schemes
> consist of three algorithms.
> \
> + keygen :: Given a security parameter generates a public private key pair.
> + encrypt :: Given a public key and a message, encrypts the message under the given public key.
> + decrypt :: Given a private key and a ciphertext, =decrypt= returns a readable plaintext if the private key was paired with the ciphertexts encryption key.
Solution "Public Key Cryptography" solves infosec in std {

  > The underlying mathematical algorithm that provides the asymmetric cipher used.
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

  > The implementation in Code
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

  > There are two keys in use.
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

  > Different algorithm's will have different security parameters, and may allow different key lengths.
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
  }
}
"""

-- --------------------------------------------------------------------- [ EOF ]
