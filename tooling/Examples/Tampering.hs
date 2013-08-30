module Examples.Tampering where

import Model

-- Tampering

tamper = Plang "Tampering" "tampering" tamperImps tamperPatts

-- ----------------------------------------------------------------- [ Imports ]
tamperImps = Just [ Import "privilegeescalation" (Pattern "distrep" "distrep" Nothing Nothing Nothing Nothing tamperRelDistRep),
                    Import "privilegeescalation" (Pattern "exedom" "exedom" Nothing Nothing Nothing Nothing tamperRelExeDom)]

-- ---------------------------------------------------------------- [ Patterns ]
tamperPatternTP  = Pattern "Trust Partitioning"                     "tp"    Nothing Nothing Nothing Nothing Nothing 
tamperPatternCS  = Pattern "Checkpointed System"                    "cs"    Nothing Nothing Nothing Nothing Nothing 
tamperPatternSDS = Pattern "Safe Data Structure"                    "sds"   Nothing Nothing Nothing Nothing Nothing 
tamperPatternSS  = Pattern "Server Sandbox"                         "ss"    Nothing Nothing Nothing Nothing tamperRelSS 
tamperPatternCJ  = Pattern "Chroot Jail"                            "cj"    Nothing Nothing Nothing Nothing Nothing 
tamperPatternUL  = Pattern "Unique Location for Each Write Request" "ulewr" Nothing Nothing Nothing Nothing Nothing 

tamperPatterns = [tamperPatternTP ,
                  tamperPatternCS ,
                  tamperPatternSDS, 
                  tamperPatternSS ,
                  tamperPatternCJ ,
                  tamperPatternUL ]

-- --------------------------------------------------------------- [ Relations ]

tamperRelDistRep = Just [ Relation "tp"    (Just "Communicate between processes"),
                          Relation "ulewr" (Just "Concurrent write race"),
                          Relation "ss"    (Just "Constrain Environment")]

tamperRelExeDom = Just [ Relation "exedom"  ["ss"]    (Just "Constrain Environment"),
                         Relation "exedom"  ["sds"]   (Just "Data safety"),
                         Relation "exedom"  ["cs"]    (Just "Graceful restart")]
