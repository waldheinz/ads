
module Freenet.Keys (
  DataRequest(..), toDataRequest, dataRequestLocation,
  ) where

import Freenet.Types
import Freenet.Ssk
import Freenet.URI

toDataRequest :: URI -> DataRequest
toDataRequest (CHK l _ e _ )      = ChkRequest l $ chkExtraCrypto e
toDataRequest (SSK hpk ck e dn _) = SskRequest hpk (sskEncryptDocname ck dn) $ sskExtraCrypto e

dataRequestLocation :: DataRequest -> Key
dataRequestLocation (ChkRequest l _) = l
dataRequestLocation (SskRequest hpk ehd _) = sskLocation' hpk ehd 

