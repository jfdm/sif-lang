-- ----------------------------------------- [ InformationSecrecy.idr<Problem> ]
-- Module    : InformationSecrecy.idr<Problem>
-- Copyright : (c) Jan de Muijnck-Hughes
-- License   : see LICENSE
-- --------------------------------------------------------------------- [ EOH ]

module Examples.InformationSecrecy

import Sif.Pattern

-- ------------------------------------------------------ [ Problem Definition ]

data_confidentiality : FUNCTIONAL
data_confidentiality = mkFunctional "Data Confidentiality" (Just "A passive man in the middle, eavesdropping on the untrusted channel, should not be able to extract information on the contents of the intercepted messages. He may however obtain traffic information, i.e. knowledge about the (claimed) identity of sender and/or recipient, the time of sending or receiving, the size of the transmitted data, etc")

recipient_confidentiality : FUNCTIONAL
recipient_confidentiality = mkFunctional "REcipient Confidentiality" (Just "The recipient of the message should be the only entity capable of accessing the data contained within the message.")

ttl_security : PERFORMANCE
ttl_security = mkPerformance "Suitable Security Level" (Just "The data should be kept secure for the period of time that knowledge of the data is considered harmful.")

mech_perf : PERFORMANCE
mech_perf = mkPerformance "Suitable performance" (Just "The mechanism that enables the confidentiality of data should not hinder upon use of the mechanism.")

infosec : PROBLEM
infosec = mkProblem "Information Secrecy" Nothing
  [data_confidentiality
  ,recipient_confidentiality
  ,ttl_security
  ,mech_perf]


-- --------------------------------------------------------------------- [ EOF ]
