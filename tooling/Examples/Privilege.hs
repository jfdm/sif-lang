module Examples.Privilege where


import Model


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
