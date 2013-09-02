-- --------------------------------------------------------------- [ Tampering ]
module Examples.Tampering where

-- | Tampering Pattern Language
--
-- Munawar Hafiz, Paul Adamczyk, and Ralph E. Johnson. 2012. Growing a
-- pattern language (for security). In Proceedings of the ACM
-- international symposium on New ideas, new paradigms, and
-- reflections on programming and software (Onward!  '12). ACM, New
-- York, NY, USA, 139-158. DOI=10.1145/2384592.2384607

import Model

-- | Pattern Language Definition
tamper = Plang "Tampering" "tampering" (tamperLocalPatterns ++ tamperImportedPatterns)

-- | Imported Patterns
tamperImportedPatterns = [
  Pattern "distrep" "distrep" (Just "privilegeescalation") Nothing Nothing Nothing Nothing tamperRelDistRep,
  Pattern "exedom" "exedom" (Just "privilegeescalation") Nothing Nothing Nothing Nothing tamperRelExeDom]

-- | Local Pattern Defintion
tamperLocalPatterns = [
  Pattern "Trust Partitioning"                     "tp"    Nothing Nothing Nothing Nothing Nothing Nothing,
  Pattern "Checkpointed System"                    "cs"    Nothing Nothing Nothing Nothing Nothing Nothing, 
  Pattern "Safe Data Structure"                    "sds"   Nothing Nothing Nothing Nothing Nothing Nothing,
  Pattern "Server Sandbox"                         "ss"    Nothing Nothing Nothing Nothing tamperRelSS Nothing,
  Pattern "Chroot Jail"                            "cj"    Nothing Nothing Nothing Nothing Nothing Nothing, 
  Pattern "Unique Location for Each Write Request" "ulewr" Nothing Nothing Nothing Nothing Nothing Nothing] 

-- | Relations Definition
tamperRelDistRep = Just [ Relation "tp"    (Just "Communicate between processes"),
                          Relation "ulewr" (Just "Concurrent write race"),
                          Relation "ss"    (Just "Constrain Environment")]

tamperRelExeDom = Just [ Relation "ss"    (Just "Constrain Environment"),
                         Relation "sds"   (Just "Data safety"),
                         Relation "cs"    (Just "Graceful restart")]

tamperRelSS = Just [Relation "cj" (Just "Sandbox Processes")]

-- --------------------------------------------------------------------- [ EOF ]
