-- -------------------------------------------------- [ InformationSecrecy.idr ]
-- Module    : InformationSecrecy.idr
-- Copyright : (c) Jan de Muijnck-Hughes
-- License   : see LICENSE
-- --------------------------------------------------------------------- [ EOH ]
module Examples.InformationSecrecy

import Sif.Pattern

data_confidentiality : FUNCTIONAL
data_confidentiality = mkFunctional "Data Confidentiality" (Just "A passive man in the middle, eavesdropping on the untrusted channel, should not be able to extract information on the contents of the intercepted messages. He may however obtain traffic information, i.e. knowledge about the (claimed) identity of sender and/or recipient, the time of sending or receiving, the size of the transmitted data, etc")

recipient_confidentiality : FUNCTIONAL
recipient_confidentiality = mkFunctional "REcipient Confidentiality" (Just "The recipient of the message should be the only entity capable of accessing the data contained within the message.")

ttl_security : PERFORMANCE
ttl_security = mkPerformance "Suitable Security Level" (Just "The data should be kept secure for the period of time that knowledge of the data is considered harmful.")

mech_perf : PERFORMANCE
mech_perf = mkPerformance "Suitable performance" (Just "The mechanism that enables the confidentiality of data should not hinder upon use of the mechanism.")


infosec : PROBLEM
infosec = mkProblem "Information Secrecy" Nothing
  [data_confidentiality
  ,recipient_confidentiality
  ,ttl_security
  ,mech_perf]

namespace AsymmetricCrypto
  keypairs : PROPERTY
  keypairs = mkProperty "Public Key Pairs" Nothing [t1,t2,t3]
    where
      t1 : ADVANTAGE
      t1 = mkAdvantage "Different Key Pairs" Nothing SATISFIED
              [mkLink MAKES recipient_confidentiality]

      t2 : DISADVANTAGE
      t2 = mkDisadvantage "PKI" Nothing SATISFIED
              [mkLink HURTS ttl_security]

      t3 : ADVANTAGE
      t3 = mkAdvantage "Public Key Pair" Nothing SATISFIED
              [mkLink HELPS data_confidentiality]

  maths : PROPERTY
  maths = mkProperty "Maths Algorithm" Nothing [t1,t2,t3]
    where
      t1 : ADVANTAGE
      t1 = mkAdvantage "Maths Guarantee" Nothing SATISFIED
              [mkLink MAKES data_confidentiality]

      t2 : DISADVANTAGE
      t2 = mkDisadvantage "Computationally Expensive" Nothing UNKNOWN
              [mkLink HURTS mech_perf]

      t3 : ADVANTAGE
      t3 = mkAdvantage "Maths well studied" Nothing SATISFIED
              [mkLink HELPS recipient_confidentiality]

   secparam : PROPERTY
   secparam = mkProperty "Variable Security Parameter" Nothing [t1,t2,t3]
     where
       t1 : ADVANTAGE
       t1 = mkAdvantage "Greater security" Nothing SATISFIED
               [mkLink MAKES ttl_security]

       t2 : DISADVANTAGE
       t2 = mkDisadvantage "Parameter selection" Nothing SATISFIED
               [mkLink HURTS ttl_security]

       t3 : DISADVANTAGE
       t3 = mkDisadvantage "Parameter Computation" Nothing SATISFIED
               [mkLink HURTS mech_perf]

   asymCrypto : SOLUTION
   asymCrypto = mkSolution "Asymmetric Crypto" Nothing
                  [keypairs, maths, secparam]

   infoSecAsymCrypto : PATTERN
   infoSecAsymCrypto = mkPattern "Information Security using Asymmetric Crypto" Nothing infosec asymCrypto


namespace Main
  main : IO ()
  main = do
    let m = getModel infoSecAsymCrypto
    putStrLn $ prettyModel m

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


-- Describe parameters such that their inherent goodness shines through in model.
-- model consequences (good and bad aspects of actions/properties.)
  -- Model good traits bad traits.
-- solution trait implementation trait.
-- Info sec is the problem
-- Asym is the partial solution
-- RSA, DSA are the real solutions.
-- --------------------------------------------------------------------- [ EOF ]


{-


-}
