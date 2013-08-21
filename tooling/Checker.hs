module Checker where


import System.IO
import Data.List
import Data.Maybe
import Parser
import Model

-- ------------------------------------------------------- [ Testing Languages ]
tropycInfo = PLangLabel  "Tropyc" "tropyc"
tropycPatt = [ (Pattern "Generic OO Crypto Architecture" "p1" Nothing Nothing (Just "Abstract")),
               (Pattern "Information Secrecy" "infosec" (Just ["p1"]) Nothing Nothing),
               (Pattern "Sender Authentication" "senderauth" (Just ["p1"]) Nothing Nothing),
               (Pattern "Signature" "sig" (Just ["p1"]) Nothing Nothing),
               (Pattern "Message Integrity" "misgint" (Just ["p1"]) Nothing Nothing),
               (Pattern  "Secrecy with Authentication" "swa" Nothing Nothing Nothing),
               (Pattern  "Secrecy with Signature" "sws" Nothing Nothing Nothing),
               (Pattern  "Signature with Appendix" "sigapp" Nothing Nothing Nothing),
               (Pattern  "Secrecy with Integrity" "swi" Nothing Nothing Nothing),
               (Pattern  "Secrecy with Signature with Appendix" "ssa" Nothing Nothing Nothing)]

tropycRela = [ (Requires "swa" ["infosec", "senderauth"] Nothing),
               (Requires "sws" ["infosec", "sig"] Nothing),
               (Requires "sigapp" ["senderauth", "msgint"] Nothing),
               (Requires "swi" ["infosec"] Nothing),
               (Requires "swi" ["msgint"] Nothing),
               (Requires "ssa" ["sws"] Nothing),
               (Requires "ssa" ["sigapp"] Nothing)]

tropyc = PatternLang tropycInfo Nothing tropycPatt tropycRela

getPatternIDs :: PatternLang -> IDs 
getPatternIDs plang = map (Model.id) (Model.patterns plang)

getInstRelationIDs :: PatternLang -> IDs
getInstRelationIDs plang = nub $ union tos froms
                           where
                             tos = map (Model.from) (Model.relations plang)
                             froms = concatMap (Model.to) (Model.relations plang)

getClassRelationIDs :: PatternLang -> IDs
getClassRelationIDs plang = nub $ union extends implements
                            where
                              extends = concat $ mapMaybe (Model.extends) (Model.patterns plang)
                              implements = concat $ mapMaybe (Model.implements) (Model.patterns plang)


-- Checks for orphan patterns
-- Returns the list pf orphan pattern identifiers
orphanIDs :: PatternLang -> IDs
orphanIDs plang = (getPatternIDs plang) \\ r
                  where
                    r = union (getInstRelationIDs plang) (getClassRelationIDs plang)
