module Examples.Tropyc where

import Model


tropyc = Plang "Tropyc" "tropyc" Nothing tropycPatt

--                               Name                                   ID            Modifier        Extends               Realises Requires                                    Links
tropycPatternGen       = Pattern "Generic OO Crypto Architecture"       "p1"         (Just Abstract)  Nothing               Nothing  Nothing                                     Nothing
tropycPatternInfoSec   = Pattern "Information Secrecy"                  "infosec"    Nothing          (Just [tropycRelGen]) Nothing  Nothing                                     Nothing
tropycPatternSendAuth  = Pattern "Sender Authentication"                "senderauth" Nothing          (Just [tropycRelGen]) Nothing  Nothing                                     Nothing
tropycPatternSIG       = Pattern "Signature"                            "sig"        Nothing          (Just [tropycRelGen]) Nothing  Nothing                                     Nothing
tropycPatternMsgInt    = Pattern "Message Integrity"                    "msgint"     Nothing          (Just [tropycRelGen]) Nothing  Nothing                                     Nothing
tropycPatternSWA       = Pattern "Secrecy with Authentication"          "swa"        Nothing          Nothing               Nothing  (Just [tropycRelInfoSec, tropycRelInfoSec]) Nothing
tropycPatternSWS       = Pattern "Secrecy with Signature"               "sws"        Nothing          Nothing               Nothing  (Just [tropycRelInfoSec, tropycRelSIG])     Nothing
tropycPatternSigApp    = Pattern "Signature with Appendix"              "sigapp"     Nothing          Nothing               Nothing  (Just [tropycRelSA,      tropycRelMSG])     Nothing
tropycPatternSWI       = Pattern "Secrecy with Integrity"               "swi"        Nothing          Nothing               Nothing  (Just [tropycRelInfoSec, tropycRelMSG])     Nothing
tropycPatternSSA       = Pattern "Secrecy with Signature with Appendix" "ssa"        Nothing          Nothing               Nothing  (Just [tropycRelSWS,     tropycRelSigApp])  Nothing

tropycRelGen     = Relation tropycPatternGen      Nothing
tropycRelInfoSec = Relation tropycPatternInfoSec  Nothing
tropycRelSA      = Relation tropycPatternSendAuth Nothing
tropycRelSIG     = Relation tropycPatternSIG      Nothing  
tropycRelMSG     = Relation tropycPatternMsgInt   Nothing  
tropycRelSWS     = Relation tropycPatternSWS      Nothing
tropycRelSigApp  = Relation tropycPatternSigApp   Nothing

tropycPatt = [tropycPatternGen     , 
              tropycPatternInfoSec , 
              tropycPatternSendAuth, 
              tropycPatternSIG     , 
              tropycPatternMsgInt  , 
              tropycPatternSWA     , 
              tropycPatternSWS     , 
              tropycPatternSigApp  , 
              tropycPatternSWI     , 
              tropycPatternSSA      ]
