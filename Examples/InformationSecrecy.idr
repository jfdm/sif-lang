-- -------------------------------------------------- [ InformationSecrecy.idr ]
-- Module    : InformationSecrecy.idr
-- Copyright : (c) Jan de Muijnck-Hughes
-- License   : see LICENSE
-- --------------------------------------------------------------------- [ EOH ]
module Examples.InformationSecrecy

import Sif.Pattern

data_confidentiality : FUNCTIONAL
data_confidentiality = mkFunctional "A passive man in the middle, eavesdropping on the untrusted channel, should not be able to extract information on the contents of the intercepted messages. He may however obtain traffic information, i.e. knowledge about the (claimed) identity of sender and/or recipient, the time of sending or receiving, the size of the transmitted data, etc"

recipient_confidentiality : FUNCTIONAL
recipient_confidentiality = mkFunctional "The recipient of the message should be the only entity capable of accessing the data contained within the message."

ttl_security : PERFORMANCE
ttl_security = mkPerformance "The data should be kept secure for the period of time that knowledge of the data is considered harmful."

mech_perf : PERFORMANCE
mech_perf = mkPerformance "The mechanism that enables the confidentiality of data should not hinder upon use of the mechanism."


infosec : Problem
infosec = empty "Information Secrecy"
  += data_confidentiality
  += recipient_confidentiality
  += ttl_security
  += mech_perf
  += (ttl_security ~~> recipient_confidentiality | HELPS )

{-
Problem has requirements ::= Graph of goals
Solution has properties  ::= Node decomposed into sub-graphs of tasks
Property has traits      ::= Tasks
Traits affect requirements

-}
{-
+ Asymmetric Encryption

+ Solution Properties
  + Property :: Public Private Key Pairs
    + Good Trait :: Different Keys
    + Bad Trait  :: Key distribution
    + Bad Trait  :: Different Keys
  + Property :: Mathematical Algorithm
    + Good Trait :: Maths guarantee
    + Bad Trait  :: Computationally Expensive.
    + Good Trait :: Well Studied
  + Property :: Variable Security Parameter
    + Good Trait :: Larger Parameter greater security
    + Bad Trait  :: Selection of parameters
    + Bad Trait  :: Larger Parameter greater computation.

-}
namespace AsymmetricCrypto


  keypair : ACTION
  keypair = mkAction "There is a public key pair" Nothing

  keysecrecy : ACTION
  keysecrecy = mkAction "Private keys must be kept protected from unauthorised copy." Nothing

  keydistribution : ACTION
  keydistribution = mkAction "Keys need to be distributed." Nothing



-- Describe parameters such that their inherent goodness shines through in model.
-- model consequences (good and bad aspects of actions/properties.)
  -- Model good traits bad traits.
-- solution trait implementation trait.
-- Info sec is the problem
-- Asym is the partial solution
-- RSA, DSA are the real solutions.
  asymCrypto : Solution
  asymCrypto = empty "Asymmetric ENcryption"


-- --------------------------------------------------------------------- [ EOF ]


{-


-}
