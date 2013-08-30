module Examples.Tampering where

import Model

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
