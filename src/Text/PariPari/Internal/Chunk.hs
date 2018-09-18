{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
module Text.PariPari.Internal.Chunk (
  Chunk(..)
  , CharChunk(..)
  , Pos(..)
  , showByte
  , showByteString
  , unsafeAsciiToChar
  , asc_0, asc_9, asc_A, asc_E, asc_P, asc_a, asc_e, asc_p,
    asc_minus, asc_plus, asc_point, asc_newline
) where

import Data.Bits (unsafeShiftL, (.|.), (.&.))
import Data.ByteString (ByteString)
import Data.Foldable (foldl')
import Data.String (fromString)
import Data.Text (Text)
import Data.Word (Word8)
import Foreign.ForeignPtr (ForeignPtr, withForeignPtr)
import Foreign.Ptr (plusPtr)
import Foreign.Storable (peekByteOff)
import GHC.Base (unsafeChr)
import GHC.Generics (Generic)
import GHC.Show (showLitChar)
import Numeric (showHex)
import qualified Data.ByteString as B
import qualified Data.ByteString.Internal as B
import qualified Data.Text.Array as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Internal as T

-- | Line and column position starting at (1,1)
data Pos = Pos
  { _posLine   :: !Int
  , _posColumn :: !Int
  } deriving (Eq, Show, Generic)

class (Ord (Element k), Ord k) => Chunk k where
  type Element  k
  type Buffer   k
  elementAt :: Buffer k -> Int -> (Element k, Int)
  elementPos :: Element k -> Pos -> Pos
  chunkWidth :: k -> Int
  chunkEqual :: Buffer k -> Int -> k -> Bool
  packChunk :: Buffer k -> Int -> Int -> k
  unpackChunk :: k -> (Buffer k, Int, Int)
  showElement :: Element k -> String
  showChunk :: k -> String

class Chunk k => CharChunk k where
  byteAt :: Buffer k -> Int -> Word8
  charAt :: Buffer k -> Int -> (Char, Int)
  charAtFixed :: Int -> Buffer k -> Int -> Char
  charWidth :: Char -> Int
  stringToChunk :: String -> k

instance Chunk ByteString where
  type Element  ByteString = Word8
  type Buffer   ByteString = ForeignPtr Word8

  elementAt b i = (ptrByteAt b i, 1)
  {-# INLINE elementAt #-}

  elementPos e (Pos l c) =
    Pos (if e == asc_newline then l + 1 else l)
        (if e == asc_newline then 1 else c + 1)
  {-# INLINE elementPos #-}

  chunkWidth (B.PS _ _ n) = n
  {-# INLINE chunkWidth #-}

  chunkEqual b i (B.PS p j n) = ptrBytesEqual b i p j n
  {-# INLINE chunkEqual #-}

  packChunk b i n = B.PS b i n
  {-# INLINE packChunk #-}

  unpackChunk k =
    let (B.PS b i n) = k <> fromString "\0\0\0" -- sentinel
    in (b, i, n - 3)
  {-# INLINE unpackChunk #-}

  showElement = showByte
  showChunk = showByteString

instance CharChunk ByteString where
  byteAt = ptrByteAt
  {-# INLINE byteAt #-}

  charAt = ptrDecodeUtf8
  {-# INLINE charAt #-}

  charWidth = charWidthUtf8
  {-# INLINE charWidth #-}

  charAtFixed = ptrDecodeFixedUtf8
  {-# INLINE charAtFixed #-}

  stringToChunk t = T.encodeUtf8 $ fromString t
  {-# INLINE stringToChunk #-}

instance Chunk Text where
  type Element  Text = Char
  type Buffer   Text = T.Array

  elementAt b i = arrayCharAt 2 b i
  {-# INLINE elementAt #-}

  elementPos e (Pos l c) =
    Pos (if e == '\n' then l + 1 else l)
        (if e == '\n' then 1 else c + 1)
  {-# INLINE elementPos #-}

  chunkWidth (T.Text _ _ n) = n
  {-# INLINE chunkWidth #-}

  chunkEqual b i (T.Text a j n) = T.equal b i a j n
  {-# INLINE chunkEqual #-}

  packChunk b i n = T.Text b i n
  {-# INLINE packChunk #-}

  unpackChunk k =
    let (T.Text b i n) = k <> fromString "\0" -- sentinel
    in (b, i, n - 1)
  {-# INLINE unpackChunk #-}

  showElement = show
  showChunk = show

instance CharChunk Text where
  byteAt = arrayByteAt
  {-# INLINE byteAt #-}

  charAt = arrayCharAt 2
  {-# INLINE charAt #-}

  charWidth = charWidthUtf16
  {-# INLINE charWidth #-}

  charAtFixed n b i = fst $ arrayCharAt n b i
  {-# INLINE charAtFixed #-}

  stringToChunk t = fromString t
  {-# INLINE stringToChunk #-}

arrayByteAt :: T.Array -> Int -> Word8
arrayByteAt a i
  | c <- T.unsafeIndex a i, c <= 0xFF = fromIntegral c
  | otherwise = 0
{-# INLINE arrayByteAt #-}

arrayCharAt :: Int -> T.Array -> Int -> (Char, Int)
arrayCharAt 1 a i
  | c <- T.unsafeIndex a i, c < 0xD800 || c > 0xDFFF = (unsafeChr $ fromIntegral c, 1)
  | otherwise = ('\0', 0)
arrayCharAt _ a i
  | hi <- T.unsafeIndex a i, lo <- T.unsafeIndex a (i + 1) =
      if hi < 0xD800 || hi > 0xDFFF then
        (unsafeChr $ fromIntegral hi, 1)
      else
        (unsafeChr $ 0x10000 + ((fromIntegral $ hi - 0xD800) `unsafeShiftL` 10) + (fromIntegral lo - 0xDC00), 2)
{-# INLINE arrayCharAt #-}

ptrBytesEqual :: ForeignPtr Word8 -> Int -> ForeignPtr Word8 -> Int -> Int -> Bool
ptrBytesEqual p1 i1 p2 i2 n =
  B.accursedUnutterablePerformIO $
  withForeignPtr p1 $ \q1 ->
  withForeignPtr p2 $ \q2 ->
  (== 0) <$> B.memcmp (q1 `plusPtr` i1) (q2 `plusPtr` i2) n
{-# INLINE ptrBytesEqual #-}

ptrByteAt :: ForeignPtr Word8 -> Int -> Word8
ptrByteAt p i = B.accursedUnutterablePerformIO $
  withForeignPtr p $ \q -> peekByteOff q i
{-# INLINE ptrByteAt #-}

at :: ForeignPtr Word8 -> Int -> Int
at p i = fromIntegral $ ptrByteAt p i
{-# INLINE at #-}

-- | Decode UTF-8 character at the given offset relative to the pointer
ptrDecodeUtf8 :: ForeignPtr Word8 -> Int -> (Char, Int)
ptrDecodeUtf8 p i
  | a1 <- at p i,
    a1 <= 0x7F =
    (unsafeChr a1, 1)
  | a1 <- at p i, a2 <- at p (i + 1),
    (a1 .&. 0xE0) == 0xC0,
    (a2 .&. 0xC0) == 0x80 =
    (unsafeChr (((a1 .&. 31) `unsafeShiftL` 6)
                .|. (a2 .&. 0x3F)), 2)
  | a1 <- at p i, a2 <- at p (i + 1), a3 <- at p (i + 2),
    (a1 .&. 0xF0) == 0xE0,
    (a2 .&. 0xC0) == 0x80,
    (a3 .&. 0xC0) == 0x80 =
    (unsafeChr (((a1 .&. 15) `unsafeShiftL` 12)
                 .|. ((a2 .&. 0x3F) `unsafeShiftL` 6)
                 .|. (a3 .&. 0x3F)), 3)
  | a1 <- at p i, a2 <- at p (i + 1), a3 <- at p (i + 2), a4 <- at p (i + 3),
    (a1 .&. 0xF8) == 0xF0,
    (a2 .&. 0xC0) == 0x80,
    (a3 .&. 0xC0) == 0x80,
    (a4 .&. 0xC0) == 0x80 =
    (unsafeChr (((a1 .&. 7) `unsafeShiftL` 18)
                 .|. ((a2 .&. 0x3F) `unsafeShiftL` 12)
                 .|. ((a3 .&. 0x3F) `unsafeShiftL` 6)
                 .|. (a4 .&. 0x3F)), 4)
  | otherwise = ('\0', 0)
{-# INLINE ptrDecodeUtf8 #-}

-- | Decode UTF-8 character with fixed width at the given offset relative to the pointer
ptrDecodeFixedUtf8 :: Int -> ForeignPtr Word8 -> Int -> Char
ptrDecodeFixedUtf8 w p i = unsafeChr $
  case w of
    1 -> at p i
    2 | a1 <- at p i, a2 <- at p (i + 1) ->
        ((a1 .&. 31) `unsafeShiftL` 6)
        .|. (a2 .&. 0x3F)
    3 | a1 <- at p i, a2 <- at p (i + 1), a3 <- at p (i + 2) ->
        ((a1 .&. 15) `unsafeShiftL` 12)
        .|. ((a2 .&. 0x3F) `unsafeShiftL` 6)
        .|. (a3 .&. 0x3F)
    4 | a1 <- at p i, a2 <- at p (i + 1), a3 <- at p (i + 2), a4 <- at p (i + 3) ->
        ((a1 .&. 7) `unsafeShiftL` 18)
        .|. ((a2 .&. 0x3F) `unsafeShiftL` 12)
        .|. ((a3 .&. 0x3F) `unsafeShiftL` 6)
        .|. (a4 .&. 0x3F)
    _ -> 0
{-# INLINE ptrDecodeFixedUtf8 #-}

charWidthUtf16 :: Char -> Int
charWidthUtf16 c | c <= unsafeChr 0xFFFF = 1
                 | otherwise = 2
{-# INLINE charWidthUtf16 #-}

-- | Bytes width of an UTF-8 character
charWidthUtf8 :: Char -> Int
charWidthUtf8 c | c <= unsafeChr 0x7F = 1
                | c <= unsafeChr 0x7FF = 2
                | c <= unsafeChr 0xFFFF = 3
                | otherwise = 4
{-# INLINE charWidthUtf8 #-}

asc_0, asc_9, asc_A, asc_E, asc_P, asc_a, asc_e, asc_p,
  asc_minus, asc_plus, asc_point, asc_newline :: Word8
asc_0 = 48
asc_9 = 57
asc_A = 65
asc_E = 69
asc_P = 80
asc_a = 97
asc_e = 101
asc_p = 112
asc_minus = 45
asc_plus = 43
asc_point = 46
asc_newline = 10

unsafeAsciiToChar :: Word8 -> Char
unsafeAsciiToChar = unsafeChr . fromIntegral
{-# INLINE unsafeAsciiToChar #-}

byteS :: Word8 -> ShowS
byteS b
  | b < 128 = showLitChar $ unsafeAsciiToChar b
  | otherwise = ("\\x" <>) . showHex b

bytesS :: ByteString -> ShowS
bytesS b | B.length b == 1 = byteS $ B.head b
         | otherwise = foldl' ((. byteS) . (.)) id $ B.unpack b

showByte :: Word8 -> String
showByte b = ('\'':) . byteS b . ('\'':) $ ""

showByteString :: ByteString -> String
showByteString b = ('"':) . bytesS b . ('"':) $ ""
