module Most.Codec where

import Most.Prelude
import qualified Data.Serialize.Get as Get
import qualified Data.Serialize.Put as Put
import qualified Data.ByteString as ByteString
import qualified Data.Scientific as Scientific
import qualified Data.IntMap.Strict as IntMap
import qualified DeferredFolds.Unfoldr as Unfoldr
import qualified Most.Folds as Folds


data Codec a = Codec { put :: a -> Put.Put, get :: Get.Get a }

instance Invariant Codec where
  invmap fn1 fn2 (Codec enc dec) = Codec (enc . fn2) (fmap fn1 dec)

encode :: Codec a -> a -> ByteString
encode (Codec enc _) = Put.runPut . enc

decode :: Codec a -> ByteString -> Either String a
decode (Codec _ dec) = Get.runGet dec

byteString :: Codec ByteString
byteString = Codec
  (\ a -> do
    put varLengthWord64 (fromIntegral (ByteString.length a))
    Put.putByteString a
  )
  (do
    length <- get varLengthWord64
    Get.getBytes (fromIntegral length)
  )

product2 :: Codec a -> Codec b -> Codec (a, b)
product2 (Codec enc1 dec1) (Codec enc2 dec2) = Codec
  (\ (a, b) -> enc1 a *> enc2 b)
  (liftA2 (,) dec1 dec2)

product3 :: Codec a -> Codec b -> Codec c -> Codec (a, b, c)
product3 (Codec enc1 dec1) (Codec enc2 dec2) (Codec enc3 dec3) = Codec
  (\ (a, b, c) -> enc1 a *> enc2 b *> enc3 c)
  ((,,) <$> dec1 <*> dec2 <*> dec3)

product4 :: Codec a -> Codec b -> Codec c -> Codec d -> Codec (a, b, c, d)
product4 (Codec enc1 dec1) (Codec enc2 dec2) (Codec enc3 dec3) (Codec enc4 dec4) = Codec
  (\ (a, b, c, d) -> enc1 a *> enc2 b *> enc3 c *> enc4 d)
  ((,,,) <$> dec1 <*> dec2 <*> dec3 <*> dec4)

product5 :: Codec a -> Codec b -> Codec c -> Codec d -> Codec e -> Codec (a, b, c, d, e)
product5 (Codec enc1 dec1) (Codec enc2 dec2) (Codec enc3 dec3) (Codec enc4 dec4) (Codec enc5 dec5) = Codec
  (\ (a, b, c, d, e) -> enc1 a *> enc2 b *> enc3 c *> enc4 d *> enc5 e)
  ((,,,,) <$> dec1 <*> dec2 <*> dec3 <*> dec4 <*> dec5)

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

sum3 :: Codec a -> Codec b -> Codec c -> Codec (Either a (Either b c))
sum3 (Codec enc1 dec1) (Codec enc2 dec2) (Codec enc3 dec3) = Codec
  (\ case
    Left a -> Put.putWord8 0 *> enc1 a
    Right (Left a) -> Put.putWord8 1 *> enc2 a
    Right (Right a) -> Put.putWord8 2 *> enc3 a
  )
  (do
    tag <- Get.getWord8
    case tag of
      0 -> fmap Left dec1
      1 -> fmap (Right . Left) dec2
      2 -> fmap (Right . Right) dec3
      _ -> fail "Unexpected tag"
  )

sum4 :: Codec a -> Codec b -> Codec c -> Codec d -> Codec (Either a (Either b (Either c d)))
sum4 (Codec enc1 dec1) (Codec enc2 dec2) (Codec enc3 dec3) (Codec enc4 dec4) = Codec
  (\ case
    Left a -> Put.putWord8 0 *> enc1 a
    Right (Left a) -> Put.putWord8 1 *> enc2 a
    Right (Right (Left a)) -> Put.putWord8 2 *> enc3 a
    Right (Right (Right a)) -> Put.putWord8 3 *> enc4 a
  )
  (do
    tag <- Get.getWord8
    case tag of
      0 -> fmap Left dec1
      1 -> fmap (Right . Left) dec2
      2 -> fmap (Right . Right . Left) dec3
      3 -> fmap (Right . Right . Right) dec4
      _ -> fail "Unexpected tag"
  )

sum5 :: Codec a -> Codec b -> Codec c -> Codec d -> Codec e -> Codec (Either a (Either b (Either c (Either d e))))
sum5 (Codec enc1 dec1) (Codec enc2 dec2) (Codec enc3 dec3) (Codec enc4 dec4) (Codec enc5 dec5) = Codec
  (\ case
    Left a -> Put.putWord8 0 *> enc1 a
    Right (Left a) -> Put.putWord8 1 *> enc2 a
    Right (Right (Left a)) -> Put.putWord8 2 *> enc3 a
    Right (Right (Right (Left a))) -> Put.putWord8 3 *> enc4 a
    Right (Right (Right (Right a))) -> Put.putWord8 4 *> enc5 a
  )
  (do
    tag <- Get.getWord8
    case tag of
      0 -> fmap Left dec1
      1 -> fmap (Right . Left) dec2
      2 -> fmap (Right . Right . Left) dec3
      3 -> fmap (Right . Right . Right . Left) dec4
      4 -> fmap (Right . Right . Right . Right) dec5
      _ -> fail "Unexpected tag"
  )

none :: Codec ()
none = Codec (const mempty) (pure ())

scientific :: Codec Scientific
scientific = 
  invmap
    (uncurry Scientific.scientific)
    (Scientific.coefficient &&& Scientific.base10Exponent)
    (product2 integer varLengthSignedIntegral)

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

{-|
Variable length representation of unsigned integers.

Uses the 8th bit of each octet to specify, whether another octet is needed.
-}
varLengthWord :: Codec Word
varLengthWord = varLengthUnsignedIntegral

int8 :: Codec Int8
int8 = Codec Put.putInt8 Get.getInt8

varLengthInt :: Codec Int
varLengthInt = varLengthSignedIntegral


-- * Containers
-------------------------

foldable :: (a -> Word64) -> (a -> Unfoldr b) -> Fold b a -> Codec b -> Codec a
foldable length unfoldr (Fold step init extract) (Codec enc dec) = Codec
  (\ a -> do
    put varLengthWord64 (length a)
    forM_ (unfoldr a) enc
  )
  (do
    length <- get varLengthWord64
    let
      loop count !state = if count > 0
        then do
          b <- dec
          loop (pred count) (step state b)
        else return (extract state)
      in loop length init
  )

intMap :: Codec Int -> Codec a -> Codec (IntMap a)
intMap keyCodec valueCodec =
  foldable (fromIntegral . IntMap.size) Unfoldr.intMapAssocs Folds.intMap
    (product2 keyCodec valueCodec)
