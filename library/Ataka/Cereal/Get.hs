module Ataka.Cereal.Get where

import Ataka.Prelude
import Ataka.Types
import Data.Serialize.Get
import qualified Data.Text.Encoding as Text


versionedRequestDecoding :: Word32 -> Get request -> Get (VersionedRequestDecoding request)
versionedRequestDecoding expectedVersion getRequest = do
  version <- getWord32le
  if version /= expectedVersion
    then return (UnsupportedVersionedRequestDecoding version)
    else do
      request <- getRequest
      return (SupportedVersionedRequestDecoding request)

byteString :: Get ByteString
byteString = getWord32le >>= getByteString . fromIntegral

text :: Get Text
text =
  getWord32le >>=
  getBytes . fromIntegral >>=
  either (fail . show) return . Text.decodeUtf8'

bool :: Get Bool
bool = do
  tag <- getWord8
  case tag of
    0 -> return False
    1 -> return True
    _ -> fail "Out of bool range"

maybe :: Get a -> Get (Maybe a)
maybe getA = do
  tag <- getWord8
  case tag of
    0 -> return Nothing
    1 -> Just <$> getA
    _ -> fail "Not a maybe"
