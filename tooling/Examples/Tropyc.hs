-- | Pattern Language for Cryptography
--
-- Braga, A., Rubira, C., and Dahab, R. 1998. Tropyc: A pattern
-- language for cryptographic object-oriented software. Chapter 16 in
-- Pattern Languages of Program Design 4 (N. Harrison, B. Foote, and
-- H. Rohnert, Eds.). Also in Procs. of PLoP'98, 

module Examples.Tropyc where

import Model

-- | Pattern Language Definition
tropyc = Plang "Tropyc" "tropyc" tropycPatterns



-- The Patterns
tropycPatterns = [
--        Name                                   ID           Origin  Modifier         Extends      Realises Requires        Links
  Pattern "Generic OO Crypto Architecture"       "p1"         Nothing (Just Abstract)  Nothing      Nothing  Nothing         Nothing, 
  Pattern "Information Secrecy"                  "infosec"    Nothing Nothing          tropycRelGen Nothing  Nothing         Nothing, 
  Pattern "Sender Authentication"                "senderauth" Nothing Nothing          tropycRelGen Nothing  Nothing         Nothing, 
  Pattern "Signature"                            "sig"        Nothing Nothing          tropycRelGen Nothing  Nothing         Nothing, 
  Pattern "Message Integrity"                    "msgint"     Nothing Nothing          tropycRelGen Nothing  Nothing         Nothing, 
  Pattern "Secrecy with Authentication"          "swa"        Nothing Nothing          Nothing      Nothing  tropycRelSWA    Nothing, 
  Pattern "Secrecy with Signature"               "sws"        Nothing Nothing          Nothing      Nothing  tropycRelSWS    Nothing, 
  Pattern "Signature with Appendix"              "sigapp"     Nothing Nothing          Nothing      Nothing  tropycRelSigApp Nothing, 
  Pattern "Secrecy with Integrity"               "swi"        Nothing Nothing          Nothing      Nothing  tropycRelSWI    Nothing, 
  Pattern "Secrecy with Signature with Appendix" "ssa"        Nothing Nothing          Nothing      Nothing  tropycRelSSA    Nothing]

-- | The Relation Groups
tropycRelGen    = Just [Relation "p1" Nothing]
tropycRelSWA    = Just [tropycRelInfoSec, tropycRelSA]
tropycRelSWS    = Just [tropycRelInfoSec, tropycRelSIG]
tropycRelSigApp = Just [tropycRelSA, tropycRelMSG]
tropycRelSWI    = Just [tropycRelInfoSec, tropycRelMSG]
tropycRelSSA    = Just [Relation "sws"    Nothing,
                        Relation "sigapp" Nothing ]

-- | Relations Individual
tropycRelInfoSec = Relation "infosec"    Nothing
tropycRelSA      = Relation "senderauth" Nothing
tropycRelSIG     = Relation "sig"        Nothing  
tropycRelMSG     = Relation "msgint"     Nothing  

-- --------------------------------------------------------------------- [ EOF ]
