{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE Rank2Types #-}
module Text.PariPari.Internal.CharCombinators (
  digitByte
  , integer
  , integer'
  , decimal
  , octal
  , hexadecimal
  , digit
  , signed
  , fractionHex
  , fractionDec
  , char'
  , notChar
  , anyChar
  , alphaNumChar
  , digitChar
  , letterChar
  , lowerChar
  , upperChar
  , symbolChar
  , categoryChar
  , punctuationChar
  , spaceChar
  , asciiChar
  , skipChars
  , takeChars
  , skipCharsWhile
  , takeCharsWhile
  , skipCharsWhile1
  , takeCharsWhile1
  , string
) where

import Control.Applicative ((<|>), optional)
import Control.Monad.Combinators (option, skipCount, skipMany)
import Text.PariPari.Internal.Chunk
import Text.PariPari.Internal.Class
import Text.PariPari.Internal.ElementCombinators ((<?>))
import Data.Text (Text)
import Data.Functor (void)
import Data.Word (Word8)
import qualified Data.Char as C

type CharP k a  = (forall p. CharParser k p => p a)

-- | Parse a digit byte for the given base.
-- Bases 2 to 36 are supported.
digitByte :: CharParser k p => Int -> p Word8
digitByte base = asciiSatisfy (isDigit base)
{-# INLINE digitByte #-}

isDigit :: Int -> Word8 -> Bool
isDigit base b
  | base >= 2 && base <= 10 = b >= asc_0 && b <= asc_0 + fromIntegral base - 1
  | base <= 36 = (b >= asc_0 && b <= asc_9)
                 || ((fromIntegral b :: Word) - fromIntegral asc_A) < fromIntegral (base - 10)
                 || ((fromIntegral b :: Word) - fromIntegral asc_a) < fromIntegral (base - 10)
  |otherwise = error "Text.PariPari.Internal.Combinators.isDigit: Bases 2 to 36 are supported"
{-# INLINE isDigit #-}

digitToInt :: Int -> Word8 -> Word
digitToInt base b
  | n <- (fromIntegral b :: Word) - fromIntegral asc_0, base <= 10 || n <= 9  = n
  | n <- (fromIntegral b :: Word) - fromIntegral asc_A, n               <= 26 = n + 10
  | n <- (fromIntegral b :: Word) - fromIntegral asc_a                        = n + 10
{-# INLINE digitToInt #-}

-- | Parse a single digit of the given base and return its value.
-- Bases 2 to 36 are supported.
digit :: CharParser k p => Int -> p Word
digit base = digitToInt base <$> asciiSatisfy (isDigit base)
{-# INLINE digit #-}

-- | Parse an integer of the given base.
-- Returns the integer and the number of digits.
-- Bases 2 to 36 are supported.
-- Digits can be separated by separator, e.g. `optional (char '_')`.
integer' :: (Num a, CharParser k p) => p sep -> Int -> p (a, Int)
integer' sep base = label (integerLabel base) $ do
  d <- digit base
  accum 1 $ fromIntegral d
  where accum !i !n = next i n <|> pure (n, i)
        next !i !n = do
          void $ sep
          d <- digit base
          accum (i + 1) $ n * fromIntegral base + fromIntegral d
{-# INLINE integer' #-}

-- | Parse an integer of the given base.
-- Bases 2 to 36 are supported.
-- Digits can be separated by separator, e.g. `optional (char '_')`.
integer :: (Num a, CharParser k p) => p sep -> Int -> p a
integer sep base = label (integerLabel base) $ do
  d <- digit base
  accum $ fromIntegral d
  where accum !n = next n <|> pure n
        next !n = do
          void $ sep
          d <- digit base
          accum $ n * fromIntegral base + fromIntegral d
{-# INLINE integer #-}

integerLabel :: Int -> String
integerLabel 2  = "binary integer"
integerLabel 8  = "octal integer"
integerLabel 10 = "decimal integer"
integerLabel 16 = "hexadecimal integer"
integerLabel b  = "integer of base " <> show b

decimal :: Num a => CharP k a
decimal = integer (pure ()) 10
{-# INLINE decimal #-}

octal :: Num a => CharP k a
octal = integer (pure ()) 8
{-# INLINE octal #-}

hexadecimal :: Num a => CharP k a
hexadecimal = integer (pure ()) 16
{-# INLINE hexadecimal #-}

-- | Parse a number with a plus or minus sign.
signed :: (Num a, CharParser k p) => p a -> p a
signed p = ($) <$> ((id <$ asciiByte asc_plus) <|> (negate <$ asciiByte asc_minus) <|> pure id) <*> p
{-# INLINE signed #-}

-- | Parse a fraction of arbitrary exponent base and coefficient base.
-- 'fractionDec' and 'fractionHex' should be used instead probably.
fraction :: (Num a, CharParser k p) => p expSep -> Int -> Int -> p digitSep -> p (a, Int, a)
fraction expSep expBase coeffBasePow digitSep = do
  let coeffBase = expBase ^ coeffBasePow
  coeff <- integer digitSep coeffBase
  void $ optional $ asciiByte asc_point
  (frac, fracLen) <- option (0, 0) $ integer' digitSep coeffBase
  expVal <- option 0 $ expSep *> signed (integer digitSep 10)
  pure (coeff * fromIntegral coeffBase ^ fracLen + frac,
        expBase,
        expVal - fromIntegral (fracLen * coeffBasePow))
{-# INLINE fraction #-}

-- | Parse a decimal fraction, returning (coefficient, 10, exponent),
-- corresponding to coefficient * 10^exponent.
-- Digits can be separated by separator, e.g. `optional (char '_')`.
fractionDec :: (Num a, CharParser k p) => p digitSep -> p (a, Int, a)
fractionDec sep = fraction (asciiSatisfy (\b -> b == asc_E || b == asc_e)) 10 1 sep <?> "fraction"
{-# INLINE fractionDec #-}

-- | Parse a hexadecimal fraction, returning (coefficient, 2, exponent),
-- corresponding to coefficient * 2^exponent.
-- Digits can be separated by separator, e.g. `optional (char '_')`.
fractionHex :: (Num a, CharParser k p) => p digitSep -> p (a, Int, a)
fractionHex sep = fraction (asciiSatisfy (\b -> b == asc_P || b == asc_p)) 2 4 sep <?> "hexadecimal fraction"
{-# INLINE fractionHex #-}

-- | Parse a case-insensitive character
char' :: CharParser k p => Char -> p Char
char' x =
  let l = C.toLower x
      u = C.toUpper x
  in satisfy (\c -> c == l || c == u)
{-# INLINE char' #-}

-- | Parse a character different from the given one.
notChar :: CharParser k p => Char -> p Char
notChar c = satisfy (/= c)
{-# INLINE notChar #-}

-- | Parse an arbitrary character.
anyChar :: CharP k Char
anyChar = satisfy (const True)
{-# INLINE anyChar #-}

-- | Parse an alphanumeric character, including Unicode.
alphaNumChar :: CharP k Char
alphaNumChar = satisfy C.isAlphaNum <?> "alphanumeric character"
{-# INLINE alphaNumChar #-}

-- | Parse a letter character, including Unicode.
letterChar :: CharP k Char
letterChar = satisfy C.isLetter <?> "letter"
{-# INLINE letterChar #-}

-- | Parse a lowercase letter, including Unicode.
lowerChar :: CharP k Char
lowerChar = satisfy C.isLower <?> "lowercase letter"
{-# INLINE lowerChar #-}

-- | Parse a uppercase letter, including Unicode.
upperChar :: CharP k Char
upperChar = satisfy C.isUpper <?> "uppercase letter"
{-# INLINE upperChar #-}

-- | Parse a space character, including Unicode.
spaceChar :: CharP k Char
spaceChar = satisfy C.isSpace <?> "space"
{-# INLINE spaceChar #-}

-- | Parse a symbol character, including Unicode.
symbolChar :: CharP k Char
symbolChar = satisfy C.isSymbol <?> "symbol"
{-# INLINE symbolChar #-}

-- | Parse a punctuation character, including Unicode.
punctuationChar :: CharP k Char
punctuationChar = satisfy C.isPunctuation <?> "punctuation"
{-# INLINE punctuationChar #-}

-- | Parse a digit character of the given base.
-- Bases 2 to 36 are supported.
digitChar :: CharParser k p => Int -> p Char
digitChar base = unsafeAsciiToChar <$> digitByte base
{-# INLINE digitChar #-}

-- | Parse a character beloning to the ASCII charset (< 128)
asciiChar :: CharP k Char
asciiChar = unsafeAsciiToChar <$> asciiSatisfy (const True)
{-# INLINE asciiChar #-}

-- | Parse a character belonging to the given Unicode category
categoryChar :: CharParser k p => C.GeneralCategory -> p Char
categoryChar cat = satisfy ((== cat) . C.generalCategory) <?> untitle (show cat)
{-# INLINE categoryChar #-}

untitle :: String -> String
untitle []     = []
untitle (x:xs) = C.toLower x : go xs
  where go [] = ""
        go (y:ys) | C.isUpper y = ' ' : C.toLower y : untitle ys
                  | otherwise   = y : ys

-- | Skip the next n characters
skipChars :: CharParser k p => Int -> p ()
skipChars n = skipCount n anyChar
{-# INLINE skipChars #-}

-- | Skip char while predicate is true
skipCharsWhile :: CharParser k p => (Char -> Bool) -> p ()
skipCharsWhile f = skipMany (satisfy f)
{-# INLINE skipCharsWhile #-}

-- | Skip at least one char while predicate is true
skipCharsWhile1 :: CharParser k p => (Char -> Bool) -> p ()
skipCharsWhile1 f = satisfy f *> skipCharsWhile f
{-# INLINE skipCharsWhile1 #-}

-- | Take the next n characters and advance the position by n characters
takeChars :: CharParser k p => Int -> p k
takeChars n = asChunk (skipChars n) <?> "string of length " <> show n
{-# INLINE takeChars #-}

-- | Take chars while predicate is true
takeCharsWhile :: CharParser k p => (Char -> Bool) -> p k
takeCharsWhile f = asChunk (skipCharsWhile f)
{-# INLINE takeCharsWhile #-}

-- | Take at least one byte while predicate is true
takeCharsWhile1 :: CharParser k p => (Char -> Bool) -> p k
takeCharsWhile1 f = asChunk (skipCharsWhile1 f)
{-# INLINE takeCharsWhile1 #-}

-- | Parse a string
string :: CharParser k p => Text -> p Text
string t = t <$ chunk (textToChunk t)
{-# INLINE string #-}