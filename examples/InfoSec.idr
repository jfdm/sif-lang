||| Deep modelling of the Infosec pattern
module InfoSec

import Sif.Pattern

-- ----------------------------------------------------------------- [ Problem ]
fone : Problem (Goal ?f1 UNKNOWN) FORCE
fone = Functional (Just "Only the named recipient should be able to read a sent message.")

ftwo : Problem (Goal ?f2 UNKNOWN) FORCE
ftwo = Performance  (Just "The operational cost associated with securing messages must not be greater than the intrinsic value of the message.")

fthree : Problem (Goal ?f3 UNKNOWN) FORCE
fthree = Reliability (Just "Messages should be kept secure for a length of time associated with the intrinsic value of the message.")

ffour : Problem (Goal ?f4 UNKNOWN) FORCE
ffour = Performance (Just "Different levels of protection can be specified when protecting data.")

ffive : Problem (Goal ?f5 UNKNOWN) FORCE
ffive = Usability (Just "The protection afforded to data must not affect the application of protection.")

f1 = proof search
f2 = proof search
f3 = proof search
f4 = proof search
f5 = proof search

infosec : Problem (GRLSpec ?pgrl Nil) PSPEC
infosec =  MkProblem
  (Just "How can Alice send a message to Bob in such a way that Eve cannot possibly read its content?")
  [fone, ftwo, fthree, ffour, ffive]
  Nil

pgrl = proof search

fsix : Problem (Goal ?f6 UNKNOWN) FORCE
fsix = Usability (Just "The protection afforded to data must not affect the application of protection.")

f6 = proof search

-- ---------------------------------------------------------------- [ Solution ]

-- [ Maths ]
maths : Pattern infosec (Task ?mn SATISFIED) ACT
maths = Act (Just "Mathematical soundness") SATISFIED

mn = proof search

mathProperty : Pattern {ty=MODEL} infosec ?mm PROPERTY
mathProperty = Property "Maths"
      [maths]
      [ActsOn maths SOMEPOS ffive
      ,ActsOn maths SOMEPOS ffour
      ,ActsOn maths SOMEPOS fthree
      ,ActsOn maths SOMEPOS ftwo
      ,ActsOn maths MAKES fone]

mm = proof search

-- [ Key Pair ]

kpair : Pattern infosec (Task ?kn SATISFIED) ACT
kpair = Act (Just "Public and Private Key") SATISFIED

kn = proof search

kpairProperty : Pattern {ty=MODEL} infosec ?kpm PROPERTY
kpairProperty = Property "Different Keys"
  [kpair]
  [ActsOn kpair MAKES fone]

kpm = proof search

-- [ Key Lengths ]

vklen : Pattern infosec (Task ?vn SATISFIED) ACT
vklen = Act (Just "Variable Key Length") SATISFIED

vn = proof search

klenProperty : Pattern {ty=MODEL} infosec ?klm PROPERTY
klenProperty = Property "Key Lengths"
  [vklen]
  [ActsOn vklen MAKES ffour
  ,ActsOn vklen HELPS ftwo
  ,ActsOn vklen HELPS ffive]

klm = proof search

-- [ Solution ]

asymCrypto : Pattern {ty=MODEL} infosec ?huh SPEC
asymCrypto = MkPattern (Just "AsymCrypto") [mathProperty, kpairProperty ,klenProperty]

huh = proof search


-- --------------------------------------------------------------------- [ EOF ]
