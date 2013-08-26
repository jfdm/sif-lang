module Examples where

import Model

-- ------------------------------------------------------- [ Testing Languages ]

tropyc = Plang tropycInfo Nothing tropycPatt

tropycInfo = Metadata  "Tropyc" "tropyc"
--                               Name                                   ID            Modifier         Extends               Realises Requires                                    Links
tropycPatternGen       = Pattern "Generic OO Crypto Architecture"       "p1"         (Just "Abstract") Nothing               Nothing  Nothing                                     Nothing
tropycPatternInfoSec   = Pattern "Information Secrecy"                  "infosec"    Nothing           (Just [tropycRelGen]) Nothing  Nothing                                     Nothing
tropycPatternSendAuth  = Pattern "Sender Authentication"                "senderauth" Nothing           (Just [tropycRelGen]) Nothing  Nothing                                     Nothing
tropycPatternSIG       = Pattern "Signature"                            "sig"        Nothing           (Just [tropycRelGen]) Nothing  Nothing                                     Nothing
tropycPatternMsgInt    = Pattern "Message Integrity"                    "msgint"     Nothing           (Just [tropycRelGen]) Nothing  Nothing                                     Nothing
tropycPatternSWA       = Pattern "Secrecy with Authentication"          "swa"        Nothing           Nothing               Nothing  (Just [tropycRelInfoSec, tropycRelInfoSec]) Nothing
tropycPatternSWS       = Pattern "Secrecy with Signature"               "sws"        Nothing           Nothing               Nothing  (Just [tropycRelInfoSec, tropycRelSIG])     Nothing
tropycPatternSigApp    = Pattern "Signature with Appendix"              "sigapp"     Nothing           Nothing               Nothing  (Just [tropycRelSA,      tropycRelMSG])     Nothing
tropycPatternSWI       = Pattern "Secrecy with Integrity"               "swi"        Nothing           Nothing               Nothing  (Just [tropycRelInfoSec, tropycRelMSG])     Nothing
tropycPatternSSA       = Pattern "Secrecy with Signature with Appendix" "ssa"        Nothing           Nothing               Nothing  (Just [tropycRelSWS,     tropycRelSigApp])  Nothing

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

-- ------------------------------------------------------------ [ With Imports ]

-- Tampering

-- tamper = Plang tamperInfo tamperImps tamperPatts tamperRels

-- tamperInfo = Metadata "Tampering" "tampering"

-- tamperImps = Just [ Import "privilegeescalation" (Just "distrep"),
--                     Import "privilegeescalation" (Just "exedom")]

-- tamperPatts = [ Pattern "Trust Partitioning"                     "tp"    Nothing Nothing Nothing,
--                 Pattern "Checkpointed System"                    "cs"    Nothing Nothing Nothing,
--                 Pattern "Safe Data Structure"                    "sds"   Nothing Nothing Nothing,
--                 Pattern "Server Sandbox"                         "ss"    Nothing Nothing Nothing,
--                 Pattern "Chroot Jail"                            "cj"    Nothing Nothing Nothing,
--                 Pattern "Unique Location for Each Write Request" "ulewr" Nothing Nothing Nothing]

-- tamperRels = [ Links "distrep" ["tp"]    (Just "Communicate between processes"),
--                Links "distrep" ["ulewr"] (Just "Concurrent write race"),
--                Links "distrep" ["ss"]    (Just "Constrain Environment"),
--                Links "exedom"  ["ss"]    (Just "Constrain Environment"),
--                Links "exedom"  ["sds"]   (Just "Data safety"),
--                Links "exedom"  ["cs"]    (Just "Graceful restart"),
--                Links "ss"      ["cj"]    (Just "Sandbox Processes")]

-- -- Privilege Escalation

-- privesc = Plang privInfo Nothing privPatts privRels 

-- privInfo = Metadata "Privilege Escalation" "privilegeescalation"

-- privPatts  = [ Pattern "Compartmentalisation"             "comp"    Nothing Nothing Nothing,
--                Pattern "Distributed Responsiblitly"       "distrep" Nothing Nothing Nothing,
--                Pattern "Execution Domain"                 "exedom"  Nothing Nothing Nothing,
--                Pattern "Controlled Process Creation"      "cpc"     Nothing Nothing Nothing,
--                Pattern "Secure Resource Pooling"          "srp"     Nothing Nothing Nothing,
--                Pattern "Controlled Object Factory"        "cof"     Nothing Nothing Nothing,
--                Pattern "Controlled Virtual Address Space" "cvas"    Nothing Nothing Nothing]

-- privRels = [ Links "comp"    ["distrep"] (Just "Efficient Partition"),
--              Links "distrep" ["comp"]    (Just "Re-Partition"),
--              Links "distrep" ["exedom"]  (Just "Restrict Resources"),
--              Links "exedom"  ["cpc"]     (Just "Restrict Privilege Inheritance"),
--              Links "exedom"  ["cof"]     (Just "Protect Object Rights"),
--              Links "exedom"  ["cvas"]    (Just "Protect Virtual Memory"),
--              Links "cpc"     ["srp"]     (Just "Limit Process Lifetime")]

-- --------------------------------------------------------------------- [ EOF ]
