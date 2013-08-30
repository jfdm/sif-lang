-- ---------------------------------------------------- [ Privilege Escalation ]
module Examples.Privilege where

-- | Privilege Escalation Pattern Language
--
-- Munawar Hafiz, Paul Adamczyk, and Ralph E. Johnson. 2012. Growing a
-- pattern language (for security). In Proceedings of the ACM
-- international symposium on New ideas, new paradigms, and
-- reflections on programming and software (Onward!  '12). ACM, New
-- York, NY, USA, 139-158. DOI=10.1145/2384592.2384607

import Model

-- | Pattern Language Definition
privilege = Plang "Privilege Escalation" "privilegeescalation" privilegePatterns


-- | Patterns Definition
privilegePatterns = [
  Pattern "Compartmentalisation"             "comp"    Nothing Nothing Nothing Nothing Nothing privilegeRelComp,  
  Pattern "Distributed Responsiblitly"       "distrep" Nothing Nothing Nothing Nothing Nothing privilegeRelDist,  
  Pattern "Execution Domain"                 "exedom"  Nothing Nothing Nothing Nothing Nothing privilegeRelExed,  
  Pattern "Controlled Process Creation"      "cpc"     Nothing Nothing Nothing Nothing Nothing privilegeRelCPCc,  
  Pattern "Secure Resource Pooling"          "srp"     Nothing Nothing Nothing Nothing Nothing Nothing,  
  Pattern "Controlled Object Factory"        "cof"     Nothing Nothing Nothing Nothing Nothing Nothing,  
  Pattern "Controlled Virtual Address Space" "cvas"    Nothing Nothing Nothing Nothing Nothing Nothing]


-- | Relations Definition
privilegeRelComp = Just [Relation "distrep" (Just "Efficient Partition")]
privilegeRelDist = Just [Relation "comp"    (Just "Re-Partition"),
                         Relation "exedom"  (Just "Restrict Resources")]
privilegeRelExed = Just [Relation "cpc"     (Just "Restrict Privilege Inheritance"),
                         Relation "cof"     (Just "Protect Object Rights"),
                         Relation "cvas"    (Just "Protect Virtual Memory")]
privilegeRelCPCc = Just [Relation "srp"     (Just "Limit Process Lifetime")]

-- --------------------------------------------------------------------- [ EOF ]
