module Text.PariPari.Ascii (
  module Text.PariPari.Ascii
) where

import Data.ByteString (ByteString)
import Data.Foldable (foldl')
import Data.Word (Word8)
import GHC.Base (unsafeChr)
import GHC.Show (showLitChar)
import Numeric (showHex)
import qualified Data.ByteString as B

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

showBytes :: ByteString -> String
showBytes b = ('"':) . bytesS b . ('"':) $ ""
