module Most.Codec where

import Most.Prelude
import qualified Data.Serialize.Get as Get
import qualified Data.Serialize.Put as Put
import qualified Data.ByteString as ByteString
import qualified Data.Scientific as Scientific


data Codec a = Codec (a -> Put.Put) (Get.Get a)

instance Invariant Codec where
  invmap fn1 fn2 (Codec enc dec) = Codec (enc . fn2) (fmap fn1 dec)

encode :: Codec a -> a -> ByteString
encode (Codec enc _) = Put.runPut . enc

decode :: Codec a -> ByteString -> Either String a
decode (Codec _ dec) = Get.runGet dec

{-|
ByteString of length lesser than 2^32.
-}
byteString :: Codec ByteString
byteString = Codec
  (\ a ->
    Put.putWord32be (fromIntegral (ByteString.length a)) <>
    Put.putByteString a
  )
  (do
    length <- Get.getWord32be
    Get.getBytes (fromIntegral length)
  )

product2 :: Codec a -> Codec b -> Codec (a, b)
product2 (Codec enc1 dec1) (Codec enc2 dec2) = Codec
  (\ (a, b) -> enc1 a *> enc2 b)
  (liftA2 (,) dec1 dec2)

sum2 :: Codec a -> Codec b -> Codec (Either a b)
sum2 (Codec enc1 dec1) (Codec enc2 dec2) = Codec
  (\ case
    Left a -> do
      Put.putWord8 0
      enc1 a
    Right a -> Put.putWord8 1 *> enc2 a
  )
  (do
    tag <- Get.getWord8
    case tag of
      0 -> fmap Left dec1
      1 -> fmap Right dec2
      _ -> fail "Unexpected tag"
  )

scientific :: Codec Scientific
scientific = 
  invmap
    (uncurry Scientific.scientific)
    (Scientific.coefficient &&& Scientific.base10Exponent)
    (product2 integer varLengthSignedIntegral)

{-|
Variable length representation of unsigned integers.

Uses the 8th bit of each octet to specify, whether another octet is needed.
-}
varLengthWord :: Codec Word
varLengthWord = varLengthUnsignedIntegral

{-|
Variable length representation of unsigned integers.

Uses the 8th bit of each octet to specify, whether another octet is needed.

__Warning:__
It is your responsibility to ensure that the value is non-negative,
otherwise the encoder will fall into an infinite loop.
-}
varLengthUnsignedIntegral :: (Integral a, Bits a) => Codec a
varLengthUnsignedIntegral = Codec
  (let
    loop !state = let
      nextState = unsafeShiftR state 7
      in if nextState == 0
        then Put.putWord8 (fromIntegral state)
        else do
          Put.putWord8 (setBit (fromIntegral state) 7)
          loop nextState
    in loop
  )
  (let
    loop !index !state = do
      byte <- Get.getWord8
      if testBit byte 7
        then let
          nextState = unsafeShiftL (fromIntegral (clearBit byte 7)) index .|. state
          in loop (index + 7) nextState
        else return (state .|. unsafeShiftL (fromIntegral byte) index)
    in loop 0 0
  )

{-|
Variable length representation of signed integers.

Uses the first bit to define the sign and
the 8th bit of each octet to specify, whether another octet is needed.

Unlike `varLengthUnsignedIntegral`, this codec is safe to use on all integral numbers,
including the var-sized `Integer`.
-}
varLengthSignedIntegral :: Integral a => Codec a
varLengthSignedIntegral = invmap fromIntegral toInteger integer

integer :: Codec Integer
integer = Codec
  (let
    start negative state = let
      nextState = unsafeShiftR state 6
      shifted = unsafeShiftL (fromIntegral state) 1
      signed = if negative then setBit shifted 0 else shifted
      in if nextState == 0
        then Put.putWord8 signed
        else do
          Put.putWord8 (setBit signed 7)
          loop nextState
    loop !state = let
      nextState = unsafeShiftR state 7
      in if nextState == 0
        then Put.putWord8 (fromIntegral state)
        else do
          Put.putWord8 (setBit (fromIntegral state) 7)
          loop nextState
    in \ a -> if a < 0
      then start True (abs a)
      else start False a
  )
  (let
    loop !index !state = do
      byte <- Get.getWord8
      if testBit byte 7
        then let
          nextState = unsafeShiftL (fromIntegral (clearBit byte 7)) index .|. state
          in loop (index + 7) nextState
        else return (state .|. unsafeShiftL (fromIntegral byte) index)
    in do
      byte <- Get.getWord8
      absolute <- if testBit byte 7
        then loop 6 (fromIntegral (unsafeShiftR (clearBit byte 7) 1))
        else return (fromIntegral (unsafeShiftR byte 1))
      return (if testBit byte 0 then negate absolute else absolute)
  )

word8 :: Codec Word8
word8 = Codec Put.putWord8 Get.getWord8

word64 :: Codec Word64
word64 = Codec Put.putWord64be Get.getWord64be

varLengthWord64 :: Codec Word64
varLengthWord64 = varLengthUnsignedIntegral

int8 :: Codec Int8
int8 = Codec Put.putInt8 Get.getInt8
