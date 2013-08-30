module Examples.Privilege where

import Model

-- Privilege Escalation
privilege = Plang "Privilege Escalation" "privilegeescalation" Nothing privPatts


-- --------------------------------------------------------------- [ Relations ]

privilegeRelComp = Just [Relation "distrep" (Just "Efficient Partition")]
privilegeRelDist = Just [Relation "comp"    (Just "Re-Partition"),
                         Relation "exedom"  (Just "Restrict Resources")]
privilegeRelExed = Just [Relation "cpc"     (Just "Restrict Privilege Inheritance"),
                         Relation "cof"     (Just "Protect Object Rights"),
                         Relation "cvas"    (Just "Protect Virtual Memory")]
privilegeRelCPCc = Just [Relation "srp"     (Just "Limit Process Lifetime")]

-- ---------------------------------------------------------------- [ Patterns ]
privilegePatterns = [
  Pattern "Compartmentalisation"             "comp"    Nothing Nothing Nothing Nothing privilegeRelComp,  
  Pattern "Distributed Responsiblitly"       "distrep" Nothing Nothing Nothing Nothing privilegeRelDist,  
  Pattern "Execution Domain"                 "exedom"  Nothing Nothing Nothing Nothing privilegeRelExed,  
  Pattern "Controlled Process Creation"      "cpc"     Nothing Nothing Nothing Nothing privilegeRelCPCc,  
  Pattern "Secure Resource Pooling"          "srp"     Nothing Nothing Nothing Nothing Nothing,  
  Pattern "Controlled Object Factory"        "cof"     Nothing Nothing Nothing Nothing Nothing,  
  Pattern "Controlled Virtual Address Space" "cvas"    Nothing Nothing Nothing Nothing Nothing]


-- --------------------------------------------------------------------- [ EOF ]
