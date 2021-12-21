module Main where

import qualified Data.ByteString as ByteString
import Most.Codec
import qualified Test.QuickCheck as QuickCheck
import Test.QuickCheck.Instances
import qualified Test.QuickCheck.Property as QuickCheck
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Test.Tasty.Runners
import Prelude hiding (choose)

main =
  defaultMain $
    testGroup "" $
      [ testCodec "vector" (Most.Codec.vector @Vector word8),
        testCodec "utcTime" utcTime,
        testCodec "byteString" byteString,
        testCodec "intMap" (intMap varLengthInt word8),
        testCodec "scientific" scientific,
        testCodec "word8" word8,
        testCodec "varLengthWord64" varLengthWord64,
        testCodec "varLengthWord" varLengthWord,
        testCodec "varLengthInt" varLengthInt,
        testCodec "integer" integer
      ]

testCodec :: (Arbitrary a, Show a, Eq a) => TestName -> Codec a -> TestTree
testCodec testName codec = testProperty testName $ \val ->
  Right val === decode codec (encode codec val)

tracing :: (a -> String) -> a -> a
tracing fn a = trace (fn a) a

showBits :: ByteString -> String
showBits = ByteString.unpack >>> concatMap showIntBits

showIntBits :: FiniteBits a => a -> String
showIntBits a = finiteBitSize a & pred & enumFromTo 0 & map (testBit a >>> bool '0' '1') & reverse
