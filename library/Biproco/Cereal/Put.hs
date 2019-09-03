module Biproco.Cereal.Put where

import Biproco.Prelude
import Biproco.Types
import Data.Serialize.Put
import qualified Data.ByteString as ByteString
import qualified Data.Text.Encoding as Text


versionedResponse :: Word32 -> (response -> Put) -> VersionedResponse response -> Put
versionedResponse version putResponse versionedResponse = case versionedResponse of
  SupportedVersionedResponse response -> putWord8 0 <> putResponse response
  UnsupportedVersionedResponse -> putWord8 1 <> putWord32le version
  DecodingFailureVersionedResponse details -> putWord8 2 <> text details

byteString :: ByteString -> Put
byteString x = putWord32le (fromIntegral (ByteString.length x)) <> putByteString x

text :: Text -> Put
text = byteString . Text.encodeUtf8
