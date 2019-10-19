module Main where

import Prelude hiding (choose)
import Test.QuickCheck.Instances
import Test.Tasty
import Test.Tasty.Runners
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Most.Codec
import qualified Test.QuickCheck as QuickCheck
import qualified Test.QuickCheck.Property as QuickCheck
import qualified Data.ByteString as ByteString


main = defaultMain $ testGroup "" $
  [
    testCodec "IntMap" (intMap varLengthSignedIntegral word8)
    ,
    testCodec "Scientific" scientific
    ,
    testGroup "varLengthUnsignedIntegral"
      [
        testCodec @Word8 "Word8" varLengthUnsignedIntegral
        ,
        testCodec @Word16 "Word16" varLengthUnsignedIntegral
        ,
        testCodec @Word32 "Word32" varLengthUnsignedIntegral
        ,
        testCodec @Word64 "Word64" varLengthUnsignedIntegral
        ,
        testCodec @Word "Word" varLengthUnsignedIntegral
      ]
    ,
    testGroup "varLengthSignedIntegral"
      [
        testGroup "Explicit Int8"
          (fmap
            (\ (a :: Int8) -> testCase (show a) $ let
              encoded = encode varLengthSignedIntegral a
              decoded = decode varLengthSignedIntegral encoded
              in assertEqual (showBits encoded) (Right a) decoded)
            [
              0,
              maxBound,
              minBound
            ]
          )
        ,
        testCodec @Word8 "Word8" varLengthSignedIntegral
        ,
        testCodec @Word16 "Word16" varLengthSignedIntegral
        ,
        testCodec @Word32 "Word32" varLengthSignedIntegral
        ,
        testCodec @Word64 "Word64" varLengthSignedIntegral
        ,
        testCodec @Word "Word" varLengthSignedIntegral
        ,
        testCodec @Int8 "Int8" varLengthSignedIntegral
        ,
        testCodec @Int16 "Int16" varLengthSignedIntegral
        ,
        testCodec @Int32 "Int32" varLengthSignedIntegral
        ,
        testCodec @Int64 "Int64" varLengthSignedIntegral
        ,
        testCodec @Int "Int" varLengthSignedIntegral
        ,
        testCodec @Integer "Integer" varLengthSignedIntegral
      ]
  ]

testCodec :: (Arbitrary a, Show a, Eq a) => TestName -> Codec a -> TestTree
testCodec testName codec = testProperty testName $ \ val ->
  Right val === decode codec (encode codec val)

tracing :: (a -> String) -> a -> a
tracing fn a = trace (fn a) a

showBits :: ByteString -> String
showBits = ByteString.unpack >>> concatMap showIntBits

showIntBits :: FiniteBits a => a -> String
showIntBits a = finiteBitSize a & pred & enumFromTo 0 & map (testBit a >>> bool '0' '1') & reverse
