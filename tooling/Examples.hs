module Examples where

import Model

-- ------------------------------------------------------- [ Testing Languages ]

tropyc = Plang tropycInfo Nothing tropycPatt tropycRela

tropycInfo = Metadata  "Tropyc" "tropyc"

tropycPatt = [ Pattern "Generic OO Crypto Architecture"         "p1"        Nothing       Nothing (Just "Abstract"),
               Pattern "Information Secrecy"                   "infosec"    (Just ["p1"]) Nothing Nothing,
               Pattern "Sender Authentication"                 "senderauth" (Just ["p1"]) Nothing Nothing,
               Pattern "Signature"                             "sig"        (Just ["p1"]) Nothing Nothing,
               Pattern "Message Integrity"                     "msgint"     (Just ["p1"]) Nothing Nothing,
               Pattern  "Secrecy with Authentication"          "swa"        Nothing       Nothing Nothing,
               Pattern  "Secrecy with Signature"               "sws"        Nothing       Nothing Nothing,
               Pattern  "Signature with Appendix"              "sigapp"     Nothing       Nothing Nothing,
               Pattern  "Secrecy with Integrity"               "swi"        Nothing       Nothing Nothing,
               Pattern  "Secrecy with Signature with Appendix" "ssa"        Nothing       Nothing Nothing]

tropycRela = [ Requires "swa"    ["infosec", "senderauth"] Nothing,
               Requires "sws"    ["infosec", "sig"]        Nothing,
               Requires "sigapp" ["senderauth", "msgint"]  Nothing,
               Requires "swi"    ["infosec"]               Nothing,
               Requires "swi"    ["msgint"]                Nothing,
               Requires "ssa"    ["sws"]                   Nothing,
               Requires "ssa"    ["sigapp"]                Nothing]

-- ------------------------------------------------------------ [ With Imports ]

-- Tampering

tamper = Plang tamperInfo tamperImps tamperPatts tamperRels

tamperInfo = Metadata "Tampering" "tampering"

tamperImps = Just [ Import "privilegeescalation" (Just "distrep"),
                    Import "privilegeescalation" (Just "exedom")]

tamperPatts = [ Pattern "Trust Partitioning"                     "tp"    Nothing Nothing Nothing,
                Pattern "Checkpointed System"                    "cs"    Nothing Nothing Nothing,
                Pattern "Safe Data Structure"                    "sds"   Nothing Nothing Nothing,
                Pattern "Server Sandbox"                         "ss"    Nothing Nothing Nothing,
                Pattern "Chroot Jail"                            "cj"    Nothing Nothing Nothing,
                Pattern "Unique Location for Each Write Request" "ulewr" Nothing Nothing Nothing]

tamperRels = [ Links "distrep" ["tp"]    (Just "Communicate between processes"),
               Links "distrep" ["ulewr"] (Just "Concurrent write race"),
               Links "distrep" ["ss"]    (Just "Constrain Environment"),
               Links "exedom"  ["ss"]    (Just "Constrain Environment"),
               Links "exedom"  ["sds"]   (Just "Data safety"),
               Links "exedom"  ["cs"]    (Just "Graceful restart"),
               Links "ss"      ["cj"]    (Just "Sandbox Processes")]

-- Privilege Escalation

privesc = Plang privInfo Nothing privPatts privRels 

privInfo = Metadata "Privilege Escalation" "privilegeescalation"

privPatts  = [ Pattern "Compartmentalisation"             "comp"    Nothing Nothing Nothing,
               Pattern "Distributed Responsiblitly"       "distrep" Nothing Nothing Nothing,
               Pattern "Execution Domain"                 "exedom"  Nothing Nothing Nothing,
               Pattern "Controlled Process Creation"      "cpc"     Nothing Nothing Nothing,
               Pattern "Secure Resource Pooling"          "srp"     Nothing Nothing Nothing,
               Pattern "Controlled Object Factory"        "cof"     Nothing Nothing Nothing,
               Pattern "Controlled Virtual Address Space" "cvas"    Nothing Nothing Nothing]

privRels = [ Links "comp"    ["distrep"] (Just "Efficient Partition"),
             Links "distrep" ["comp"]    (Just "Re-Partition"),
             Links "distrep" ["exedom"]  (Just "Restrict Resources"),
             Links "exedom"  ["cpc"]     (Just "Restrict Privilege Inheritance"),
             Links "exedom"  ["cof"]     (Just "Protect Object Rights"),
             Links "exedom"  ["cvas"]    (Just "Protect Virtual Memory"),
             Links "cpc"     ["srp"]     (Just "Limit Process Lifetime")]


-- --------------------------------------------------------------------- [ EOF ]
