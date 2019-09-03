module Biproco.Types where

import Biproco.Prelude


-- * Response-request server versioning
-------------------------

{-|
Decoding of a versioned request.
-}
data VersionedRequestDecoding request =
  SupportedVersionedRequestDecoding !request |
  UnsupportedVersionedRequestDecoding !Word32

{-|
Versioned response, which is determined by the version specified in the request.
-}
data VersionedResponse response =
  SupportedVersionedResponse !response |
  UnsupportedVersionedResponse |
  DecodingFailureVersionedResponse !Text
