-- -------------------------------------------------- [ InformationSecrecy.idr ]
-- Module    : InformationSecrecy.idr
-- Copyright : (c) Jan de Muijnck-Hughes
-- License   : see LICENSE
-- --------------------------------------------------------------------- [ EOH ]

||| Modelling Information Secrecy using Asymmetric Crypto.
module Examples.InformationSecrecy.AsymCrypto

import Sif.Pattern
import Examples.InformationSecrecy

-- ---------------------------------------------- [ Asymmetric Crypto Solution ]

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

-- -------------------------------------------------------------------- [ Main ]

namespace Main
  main : IO ()
  main = do
    let m = getModel infoSecAsymCrypto
    putStrLn $ show @{xml} $ toXML infoSecAsymCrypto


-- --------------------------------------------------------------------- [ EOF ]
