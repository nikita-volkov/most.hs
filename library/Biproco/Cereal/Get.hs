module Biproco.Cereal.Get where

import Biproco.Prelude
import Biproco.Types
import Data.Serialize.Get


versionedRequestDecoding :: Word32 -> Get request -> Get (VersionedRequestDecoding request)
versionedRequestDecoding expectedVersion getRequest = do
  version <- getWord32le
  if version /= expectedVersion
    then return (UnsupportedVersionedRequestDecoding version)
    else do
      request <- getRequest
      return (SupportedVersionedRequestDecoding request)
