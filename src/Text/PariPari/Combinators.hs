module Text.PariPari.Combinators (
  -- * Basics
  Text
  , F.void
  , (A.<|>)
  , A.empty
  , A.optional
  , A.many

  -- * Control.Monad.Combinators.NonEmpty
  , NonEmpty(..)
  , ON.some
  , ON.endBy1
  , ON.someTill
  , ON.sepBy1
  , ON.sepEndBy1

  -- * Control.Monad.Combinators
  , O.between
  , O.choice
  , O.count
  , O.count'
  , O.eitherP
  , O.endBy
  , O.manyTill
  , O.option
  , O.sepBy
  , O.sepEndBy
  , O.skipMany
  , O.skipSome
  , O.skipCount
  , O.skipManyTill
  , O.skipSomeTill

  -- * PariPari
  , (<?>)
  , getLine
  , getColumn
  , withPos
  , withSpan
  , getRefColumn
  , getRefLine
  , withRefPos
  , align
  , indented
  , line
  , linefold
  , notByte
  , anyByte
  , digitByte
  , asciiByte
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
  , string
  , string'
  , asString
  , takeString
) where

import Control.Applicative ((<|>))
import Control.Monad (when)
import Control.Monad.Combinators (option, skipCount)
import Data.List.NonEmpty (NonEmpty(..))
import Text.PariPari.Ascii
import Text.PariPari.Class
import Data.Text (Text)
import Prelude hiding (getLine)
import qualified Control.Applicative as A
import qualified Control.Monad.Combinators as O
import qualified Control.Monad.Combinators.NonEmpty as ON
import qualified Data.Char as C
import qualified Data.Functor as F
import qualified Data.Text.Encoding as T
import qualified Data.Text as T

infix 0 <?>

-- | Infix alias for 'label'
(<?>) :: MonadParser p => p a -> String -> p a
(<?>) = flip label
{-# INLINE (<?>) #-}

-- | Get line number of the reference position
getRefLine :: Parser Int
getRefLine = _posLine <$> getRefPos
{-# INLINE getRefLine #-}

-- | Get column number of the reference position
getRefColumn :: Parser Int
getRefColumn = _posColumn <$> getRefPos
{-# INLINE getRefColumn #-}

-- | Get current line number
getLine :: Parser Int
getLine = _posLine <$> getPos
{-# INLINE getLine #-}

-- | Get current column
getColumn :: Parser Int
getColumn = _posColumn <$> getPos
{-# INLINE getColumn #-}

-- | Decorate the parser result with the current position
withPos :: MonadParser p => p a -> p (Pos, a)
withPos p = do
  pos <- getPos
  ret <- p
  pure (pos, ret)
{-# INLINE withPos #-}

type Span = (Pos, Pos)

-- | Decoreate the parser result with the position span
withSpan :: MonadParser p => p a -> p (Span, a)
withSpan p = do
  begin <- getPos
  ret <- p
  end <- getPos
  pure ((begin, end), ret)
{-# INLINE withSpan #-}

-- | Parser succeeds on the same line as the reference line
line :: Parser ()
line = do
  l <- getLine
  rl <- getRefLine
  when (l /= rl) $ failWith $ EIndentOverLine rl l
{-# INLINE line #-}

-- | Parser succeeds on the same column as the reference column
align :: Parser ()
align = do
  c <- getColumn
  rc <- getRefColumn
  when (c /= rc) $ failWith $ EIndentNotAligned rc c
{-# INLINE align #-}

-- | Parser succeeds for columns greater than the current reference column
indented :: Parser ()
indented = do
  c <- getColumn
  rc <- getRefColumn
  when (c <= rc) $ failWith $ ENotEnoughIndent rc c
{-# INLINE indented #-}

-- | Parser succeeds either on the reference line or
-- for columns greater than the current reference column
linefold :: Parser ()
linefold = line <|> indented
{-# INLINE linefold #-}

-- | Parser a single byte different from the given one
notByte :: Word8 -> Parser Word8
notByte b = byteSatisfy (/= b) <?> "not " <> showByte b
{-# INLINE notByte #-}

-- | Parse an arbitrary byte
anyByte :: Parser Word8
anyByte = byteSatisfy (const True)
{-# INLINE anyByte #-}

-- | Parse a byte of the ASCII charset (< 128)
asciiByte :: Parser Word8
asciiByte = byteSatisfy (< 128)
{-# INLINE asciiByte #-}

-- | Parse a digit byte for the given base.
-- Bases 2 to 36 are supported.
digitByte :: Int -> Parser Word8
digitByte base = byteSatisfy (isDigit base)
{-# INLINE digitByte #-}

-- | Parse an integer of the given base.
-- Returns the integer and the number of digits.
-- Bases 2 to 36 are supported.
-- Digits can be separated by separator, e.g. `optional (char '_')`.
integer' :: (Num a, MonadParser p) => p sep -> Int -> p (a, Int)
integer' sep base = label (integerLabel base) $ do
  d <- digit base
  accum 1 $ fromIntegral d
  where accum !i !n = next i n <|> pure (n, i)
        next !i !n = do
          sep
          d <- digit base
          accum (i + 1) $ n * fromIntegral base + fromIntegral d
{-# INLINE integer' #-}

-- | Parse an integer of the given base.
-- Bases 2 to 36 are supported.
-- Digits can be separated by separator, e.g. `optional (char '_')`.
integer :: (Num a, MonadParser p) => p sep -> Int -> p a
integer sep base = label (integerLabel base) $ do
  d <- digit base
  accum $ fromIntegral d
  where accum !n = next n <|> pure n
        next !n = do
          sep
          d <- digit base
          accum $ n * fromIntegral base + fromIntegral d
{-# INLINE integer #-}

integerLabel :: Int -> String
integerLabel 2  = "binary integer"
integerLabel 8  = "octal integer"
integerLabel 10 = "decimal integer"
integerLabel 16 = "hexadecimal integer"
integerLabel b  = "integer of base " <> show b

decimal :: Num a => Parser a
decimal = integer (pure ()) 10
{-# INLINE decimal #-}

octal :: Num a => Parser a
octal = integer (pure ()) 8
{-# INLINE octal #-}

hexadecimal :: Num a => Parser a
hexadecimal = integer (pure ()) 16
{-# INLINE hexadecimal #-}

digitToInt :: Int -> Word8 -> Word
digitToInt base b
  | n <- (fromIntegral b :: Word) - fromIntegral asc_0, base <= 10 || n <= 9  = n
  | n <- (fromIntegral b :: Word) - fromIntegral asc_A, n               <= 26 = n + 10
  | n <- (fromIntegral b :: Word) - fromIntegral asc_a                        = n + 10
{-# INLINE digitToInt #-}

-- | Parse a single digit of the given base and return its value.
-- Bases 2 to 36 are supported.
digit :: Int -> Parser Word
digit base = digitToInt base <$> byteSatisfy (isDigit base)
{-# INLINE digit #-}

isDigit :: Int -> Word8 -> Bool
isDigit base b
  | base >= 2 && base <= 10 = b >= asc_0 && b <= asc_0 + fromIntegral base - 1
  | base <= 36 = (b >= asc_0 && b <= asc_9)
                 || ((fromIntegral b :: Word) - fromIntegral asc_A) < fromIntegral (base - 10)
                 || ((fromIntegral b :: Word) - fromIntegral asc_a) < fromIntegral (base - 10)
  |otherwise = error "Text.PariPari.Combinators.isDigit: Bases 2 to 36 are supported"
{-# INLINE isDigit #-}

-- | Parse a number with a plus or minus sign.
signed :: (Num a, MonadParser p) => p a -> p a
signed p = ($) <$> ((id <$ byte asc_plus) <|> (negate <$ byte asc_minus) <|> pure id) <*> p
{-# INLINE signed #-}

-- | Parse a fraction of arbitrary exponent base and coefficient base.
-- 'fractionDec' and 'fractionHex' should be used instead probably.
fraction :: (Num a, MonadParser p) => p expSep -> Int -> Int -> p digitSep -> p (a, Int, a)
fraction expSep expBase coeffBasePow digitSep = do
  let coeffBase = expBase ^ coeffBasePow
  coeff <- integer digitSep coeffBase
  A.optional $ byte asc_point
  (frac, fracLen) <- option (0, 0) $ integer' digitSep coeffBase
  expVal <- option 0 $ expSep *> signed (integer digitSep 10)
  pure (coeff * fromIntegral coeffBase ^ fracLen + frac,
        expBase,
        expVal - fromIntegral (fracLen * coeffBasePow))
{-# INLINE fraction #-}

-- | Parse a decimal fraction, returning (coefficient, 10, exponent),
-- corresponding to coefficient * 10^exponent.
-- Digits can be separated by separator, e.g. `optional (char '_')`.
fractionDec :: (Num a, MonadParser p) => p digitSep -> p (a, Int, a)
fractionDec sep = fraction (byteSatisfy (\b -> b == asc_E || b == asc_e)) 10 1 sep <?> "fraction"
{-# INLINE fractionDec #-}

-- | Parse a hexadecimal fraction, returning (coefficient, 2, exponent),
-- corresponding to coefficient * 2^exponent.
-- Digits can be separated by separator, e.g. `optional (char '_')`.
fractionHex :: (Num a, MonadParser p) => p digitSep -> p (a, Int, a)
fractionHex sep = fraction (byteSatisfy (\b -> b == asc_P || b == asc_p)) 2 4 sep <?> "hexadecimal fraction"
{-# INLINE fractionHex #-}

-- | Parse a case-insensitive character
char' :: Char -> Parser Char
char' x =
  let l = C.toLower x
      u = C.toUpper x
  in satisfy (\c -> c == l || c == u)
{-# INLINE char' #-}

-- | Parse a character different from the given one.
notChar :: Char -> Parser Char
notChar c = satisfy (/= c)
{-# INLINE notChar #-}

-- | Parse an arbitrary character.
anyChar :: Parser Char
anyChar = satisfy (const True)
{-# INLINE anyChar #-}

-- | Parse an alphanumeric character, including Unicode.
alphaNumChar :: Parser Char
alphaNumChar = satisfy C.isAlphaNum <?> "alphanumeric character"
{-# INLINE alphaNumChar #-}

-- | Parse a letter character, including Unicode.
letterChar :: Parser Char
letterChar = satisfy C.isLetter <?> "letter"
{-# INLINE letterChar #-}

-- | Parse a lowercase letter, including Unicode.
lowerChar :: Parser Char
lowerChar = satisfy C.isLower <?> "lowercase letter"
{-# INLINE lowerChar #-}

-- | Parse a uppercase letter, including Unicode.
upperChar :: Parser Char
upperChar = satisfy C.isUpper <?> "uppercase letter"
{-# INLINE upperChar #-}

-- | Parse a space character, including Unicode.
spaceChar :: Parser Char
spaceChar = satisfy C.isSpace <?> "space"
{-# INLINE spaceChar #-}

-- | Parse a symbol character, including Unicode.
symbolChar :: Parser Char
symbolChar = satisfy C.isSymbol <?> "symbol"
{-# INLINE symbolChar #-}

-- | Parse a punctuation character, including Unicode.
punctuationChar :: Parser Char
punctuationChar = satisfy C.isPunctuation <?> "punctuation"
{-# INLINE punctuationChar #-}

-- | Parse a digit character of the given base.
-- Bases 2 to 36 are supported.
digitChar :: Int -> Parser Char
digitChar base = unsafeAsciiToChar <$> digitByte base
{-# INLINE digitChar #-}

-- | Parse a character beloning to the ASCII charset (< 128)
asciiChar :: Int -> Parser Char
asciiChar base = unsafeAsciiToChar <$> digitByte base
{-# INLINE asciiChar #-}

-- | Parse a character belonging to the given Unicode category
categoryChar :: C.GeneralCategory -> Parser Char
categoryChar cat = satisfy ((== cat) . C.generalCategory) <?> untitle (show cat)
{-# INLINE categoryChar #-}

-- | Parse a text string
string :: Text -> Parser Text
string t = t <$ bytes (T.encodeUtf8 t)
{-# INLINE string #-}

string' :: Text -> Parser Text
string' s = asString (go s) <?> "case-insensitive \"" <> T.unpack (T.toLower s) <> "\""
  where go t
          | T.null t  = pure ()
          | otherwise = char' (T.head t) *> go (T.tail t)
{-# INLINE string' #-}

-- | Run the given parser but return the result as a 'Text' string
asString :: MonadParser p => p () -> p Text
asString p = T.decodeUtf8 <$> asBytes p
{-# INLINE asString #-}

-- | Take the next n characters and advance the position by n characters
takeString :: Int -> Parser Text
takeString n = asString (skipCount n anyChar) <?> "string of length " <> show n
{-# INLINE takeString #-}

untitle :: String -> String
untitle []     = []
untitle (x:xs) = C.toLower x : go xs
  where go [] = ""
        go (y:ys) | C.isUpper y = ' ' : C.toLower y : untitle ys
                  | otherwise   = y : ys
