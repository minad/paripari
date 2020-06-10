{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnboxedTuples #-}
module Text.PariPari.Internal.Chunk (
  Chunk(..)
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
import GHC.Base
import GHC.Word
import GHC.ForeignPtr
import GHC.Show (showLitChar)
import Numeric (showHex)
import qualified Data.ByteString as B
import qualified Data.ByteString.Internal as B
import qualified Data.Text.Array as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Internal as T

class Ord k => Chunk k where
  type Buffer k
  matchChunk :: Buffer k -> Int# -> k -> Int# -- Returns -1# if not matched
  packChunk :: Buffer k -> Int# -> Int# -> k
  unpackChunk :: k -> (# Buffer k, Int# #)
  showChunk :: k -> String
  matchChar :: Buffer k -> Int# -> Char# -> Int# -- Returns -1# if not matched
  indexChar :: Buffer k -> Int# -> (# Char#, Int# #)
  indexByte :: Buffer k -> Int# -> Word#
  stringToChunk :: String -> k

instance Chunk ByteString where
  type Buffer ByteString = ForeignPtr Word8

  matchChunk (ForeignPtr p _) i (B.PS (ForeignPtr p' _) (I# j) (I# n)) = go 0#
    where go :: Int# -> Int#
          go k | 1# <- k >=# n = n
               | w <- indexWord8OffAddr# p (i +# k),
                 w' <- indexWord8OffAddr# p' (j +# k),
                 1# <- w `neWord#` int2Word# 0#,
                 1# <- w `eqWord#` w' = go (k +# 1#)
               | otherwise = -1#
  {-# INLINE matchChunk #-}

  packChunk b i n = B.PS b (I# i) (I# n)
  {-# INLINE packChunk #-}

  unpackChunk k =
    let !(B.PS b (I# i) _) = k <> fromString "\0\0\0" -- sentinel
    in (# b, i #)
  {-# INLINE unpackChunk #-}

  showChunk = showByteString

  indexByte (ForeignPtr p _) i = indexWord8OffAddr# p i
  {-# INLINE indexByte #-}

  matchChar p i m
    | C# m <= '\x7F' =
        if | unsafeChr (at p i) == C# m -> 1#
           | otherwise -> -1#
    | C# m <= '\x7FF' =
        if | a1 <- at p i, a2 <- at p (i +# 1#),
             unsafeChr (((a1 .&. 31) `unsafeShiftL` 6)
                         .|. (a2 .&. 0x3F)) == C# m -> 2#
           | otherwise -> -1#
    | C# m <= '\xFFFF' =
        if | a1 <- at p i, a2 <- at p (i +# 1#), a3 <- at p (i +# 2#),
             unsafeChr (((a1 .&. 15) `unsafeShiftL` 12)
                         .|. ((a2 .&. 0x3F) `unsafeShiftL` 6)
                         .|. (a3 .&. 0x3F)) == C# m -> 3#
           | otherwise -> -1#
    | otherwise =
        if | a1 <- at p i, a2 <- at p (i +# 1#), a3 <- at p (i +# 2#), a4 <- at p (i +# 3#),
             unsafeChr (((a1 .&. 7) `unsafeShiftL` 18)
                         .|. ((a2 .&. 0x3F) `unsafeShiftL` 12)
                         .|. ((a3 .&. 0x3F) `unsafeShiftL` 6)
                         .|. (a4 .&. 0x3F)) == C# m -> 4#
           | otherwise -> -1#
  {-# INLINE matchChar #-}

  -- TODO detect invalid utf-8?
  indexChar p i
    | a1 <- at p i,
      a1 <= 0x7F =
      (# unsafeChr# a1, 1# #)
    | a1 <- at p i, a2 <- at p (i +# 1#),
      (a1 .&. 0xE0) == 0xC0,
      (a2 .&. 0xC0) == 0x80 =
      (# unsafeChr# (((a1 .&. 31) `unsafeShiftL` 6)
                     .|. (a2 .&. 0x3F)), 2# #)
    | a1 <- at p i, a2 <- at p (i +# 1#), a3 <- at p (i +# 2#),
      (a1 .&. 0xF0) == 0xE0,
      (a2 .&. 0xC0) == 0x80,
      (a3 .&. 0xC0) == 0x80 =
      (# unsafeChr# (((a1 .&. 15) `unsafeShiftL` 12)
                     .|. ((a2 .&. 0x3F) `unsafeShiftL` 6)
                     .|. (a3 .&. 0x3F)), 3# #)
    | a1 <- at p i, a2 <- at p (i +# 1#), a3 <- at p (i +# 2#), a4 <- at p (i +# 3#),
      (a1 .&. 0xF8) == 0xF0,
      (a2 .&. 0xC0) == 0x80,
      (a3 .&. 0xC0) == 0x80,
      (a4 .&. 0xC0) == 0x80 =
      (# unsafeChr# (((a1 .&. 7) `unsafeShiftL` 18)
                     .|. ((a2 .&. 0x3F) `unsafeShiftL` 12)
                     .|. ((a3 .&. 0x3F) `unsafeShiftL` 6)
                     .|. (a4 .&. 0x3F)), 4# #)
    | otherwise = (# '\0'#, 0# #)
  {-# INLINE indexChar #-}

  stringToChunk t = T.encodeUtf8 $ fromString t
  {-# INLINE stringToChunk #-}

instance Chunk Text where
  type Buffer Text = T.Array

  matchChunk a i (T.Text a' (I# j) (I# n)) = go 0#
    where go :: Int# -> Int#
          go k | 1# <- k >=# n = n
               | w <- T.unsafeIndex a (I# (i +# k)),
                 w' <- T.unsafeIndex a' (I# (j +# k)),
                 w /= 0, w == w' = go (k +# 1#)
               | otherwise = -1#
  {-# INLINE matchChunk #-}

  packChunk b i n = T.Text b (I# i) (I# n)
  {-# INLINE packChunk #-}

  unpackChunk k =
    let !(T.Text b (I# i) _) = k <> fromString "\0" -- sentinel
    in (# b, i #)
  {-# INLINE unpackChunk #-}

  showChunk = show

  indexByte a i
    | W16# c <- T.unsafeIndex a (I# i), 1# <- c `leWord#` (int2Word# 0xFF#) = c
    | otherwise = int2Word# 0#
  {-# INLINE indexByte #-}

  indexChar a i
    | hi <- T.unsafeIndex a (I# i), lo <- T.unsafeIndex a (I# (i +# 1#)) =
        if hi < 0xD800 || hi > 0xDFFF then
          (# unsafeChr# (fromIntegral hi), 1# #)
        else
          (# unsafeChr# (0x10000 + ((fromIntegral $ hi - 0xD800) `unsafeShiftL` 10) + (fromIntegral lo - 0xDC00)), 2# #)
  {-# INLINE indexChar #-}

  matchChar a i m
    | C# m <= '\xFFFF' =
        if | unsafeChr (fromIntegral (T.unsafeIndex a (I# i))) == C# m -> 1#
           | otherwise -> -1#
    | otherwise =
        if | hi <- T.unsafeIndex a (I# i), lo <- T.unsafeIndex a (I# (i +# 1#)),
             hi >= 0xD800, hi <= 0xDFFF,
             C# m == unsafeChr (0x10000 + ((fromIntegral $ hi - 0xD800) `unsafeShiftL` 10) +
                                (fromIntegral lo - 0xDC00)) -> 2#
           | otherwise -> -1#

  stringToChunk t = fromString t
  {-# INLINE stringToChunk #-}

at :: ForeignPtr Word8 -> Int# -> Int
at p i = fromIntegral $ W8# (indexByte @ByteString p i)
{-# INLINE at #-}

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
unsafeAsciiToChar x = unsafeChr (fromIntegral x)
{-# INLINE unsafeAsciiToChar #-}

unsafeChr# :: Int -> Char#
unsafeChr# (I# i) = chr# i
{-# INLINE unsafeChr# #-}

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
