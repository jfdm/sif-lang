-- -------------------------------------------------------------- [ Tropyc.idr ]
-- Module    : Tropyc.idr
-- Copyright : (c) Jan de Muijnck-Hughes
-- License   : see LICENSE
-- --------------------------------------------------------------------- [ EOH ]
module Example.Tropyc

import Sif.PLang

namespace Aim
  conf : FUNCTIONAL
  conf = MkFunctional "Confidentiality"

  mint : FUNCTIONAL
  mint = MkFunctional "Message Integrity"

  mauth : FUNCTIONAL
  mauth = MkFunctional "Message Authenticity"

  avail : FUNCTIONAL
  avail = MkFunctional "Availability"

aims : List (FUNCTIONAL)
aims = [conf, mint, mauth, avail]

namespace Pattern

  isec : COMPONENT
  isec = MkComponent "Information Secrecy"

  mauth : COMPONENT
  mauth = MkComponent "Message Authentication"

  sauth : COMPONENT
  sauth = MkComponent "Sender Authentication"

  mint : COMPONENT
  mint = MkComponent "Message Integrity"

ps : List (COMPONENT)
ps = [isec, mauth, sauth,mint]


tropyc : GModel
tropyc = (insertMany ps (insertMany aims emptyModel))
  \= (Pattern.isec o=> Aim.conf | MAKES)

-- Local Variables:
-- idris-packages: ("sif" "effects")
-- End:
-- --------------------------------------------------------------------- [ EOF ]
