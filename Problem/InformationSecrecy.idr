-- ----------------------------------------- [ InformationSecrecy.idr<Problem> ]
-- Module    : InformationSecrecy.idr<Problem>
-- Copyright : (c) Jan de Muijnck-Hughes
-- License   : see LICENSE
-- --------------------------------------------------------------------- [ EOH ]

||| The Problem of information secrecy
module Problem.InformationSecrecy

import Sif.Pattern

-- ------------------------------------------------------ [ Problem Definition ]

data_confidentiality : FUNCTIONAL
data_confidentiality = mkFunctional "Data Confidentiality" (Just desc)
  where
    desc : String
    desc = """A passive man in the middle, eavesdropping on the untrusted channel, should not be able to extract information on the contents of the intercepted messages. He may however obtain traffic information, i.e. knowledge about the (claimed) identity of sender and/or recipient, the time of sending or receiving, the size of the transmitted data, etc"""

recipient_confidentiality : FUNCTIONAL
recipient_confidentiality = mkFunctional "Recipient Confidentiality" (Just desc)
  where
    desc : String
    desc = """The recipient of the message should be the only entity capable of accessing the data contained within the message."""

ttl_security : PERFORMANCE
ttl_security = mkPerformance "Suitable Security Level" (Just desc)
  where
    desc : String
    desc = """The data should be kept secure for the period of time that knowledge of the data is considered harmful."""

mech_perf : PERFORMANCE
mech_perf = mkPerformance "Suitable performance" (Just desc)
  where
    desc : String
    desc = """The mechanism that enables the confidentiality of data should not hinder upon use of the mechanism."""

infosec : PROBLEM
infosec = mkProblem "Information Secrecy" (Just desc) reqs
  where
    reqs : REQUIREMENTS
    reqs = [data_confidentiality , recipient_confidentiality, ttl_security, mech_perf]

    desc : String
    desc = """How to send a message between two recipients such that only the intended recipient of the message can obtain the information."""


-- --------------------------------------------------------------------- [ EOF ]
