module Text.PariPari.Decode (
  bytesEqual
  , byteAt
  , utf8Decode
  , utf8DecodeFixed
  , utf8Width
) where

import Data.Word (Word8)
import Data.Bits (unsafeShiftL, (.|.), (.&.))
import Foreign.ForeignPtr (ForeignPtr, withForeignPtr)
import Foreign.Ptr (plusPtr)
import Foreign.Storable (peekByteOff)
import GHC.Base (unsafeChr)
import qualified Data.ByteString.Internal as B

bytesEqual :: ForeignPtr Word8 -> Int -> ForeignPtr Word8 -> Int -> Int -> Bool
bytesEqual p1 i1 p2 i2 n =
  B.accursedUnutterablePerformIO $
  withForeignPtr p1 $ \q1 ->
  withForeignPtr p2 $ \q2 ->
  (== 0) <$> B.memcmp (q1 `plusPtr` i1) (q2 `plusPtr` i2) n
{-# INLINE bytesEqual #-}

byteAt :: ForeignPtr Word8 -> Int -> Word8
byteAt p i = B.accursedUnutterablePerformIO $
  withForeignPtr p $ \q -> peekByteOff q i
{-# INLINE byteAt #-}

at :: ForeignPtr Word8 -> Int -> Int
at p i = fromIntegral $ byteAt p i
{-# INLINE at #-}

-- | Decode UTF-8 character at the given offset relative to the pointer
utf8Decode :: ForeignPtr Word8 -> Int -> (Char, Int)
utf8Decode p i
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
{-# INLINE utf8Decode #-}

-- | Decode UTF-8 character with known width at the given offset relative to the pointer
utf8DecodeFixed :: Int -> ForeignPtr Word8 -> Int -> Char
utf8DecodeFixed w p i = unsafeChr $
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
{-# INLINE utf8DecodeFixed #-}

-- | Bytes width of an UTF-8 character
utf8Width :: Char -> Int
utf8Width c | c <= unsafeChr 0x7F = 1
            | c <= unsafeChr 0x7FF = 2
            | c <= unsafeChr 0xFFFF = 3
            | otherwise = 4
{-# INLINE utf8Width #-}
