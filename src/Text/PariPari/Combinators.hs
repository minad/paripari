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
  , setRefColumn
  , setRefLine
  , saveRef
  , align
  , indented
  , line
  , linefold
  , notByte
  , anyByte
  , digitByte
  , asciiByte
  , integer
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

(<?>) :: MonadParser p => p a -> String -> p a
(<?>) = flip label
{-# INLINE (<?>) #-}

getRefLine :: Parser Int
getRefLine = _posLine <$> getRefPos
{-# INLINE getRefLine #-}

getRefColumn :: Parser Int
getRefColumn = _posColumn <$> getRefPos
{-# INLINE getRefColumn #-}

setRefLine :: Int -> Parser ()
setRefLine l = do
  c <- getRefColumn
  setRefPos $ Pos l c
{-# INLINE setRefLine #-}

setRefColumn :: Int -> Parser ()
setRefColumn c = do
  l <- getRefLine
  setRefPos $ Pos l c
{-# INLINE setRefColumn #-}

getLine :: Parser Int
getLine = _posLine <$> getPos
{-# INLINE getLine #-}

getColumn :: Parser Int
getColumn = _posColumn <$> getPos
{-# INLINE getColumn #-}

withPos :: MonadParser p => p a -> p (a, Pos)
withPos p = do
  ret <- p
  pos <- getPos
  pure (ret, pos)
{-# INLINE withPos #-}

type Span = (Pos, Pos)

withSpan :: MonadParser p => p a -> p (a, Span)
withSpan p = do
  begin <- getPos
  ret <- p
  end <- getPos
  pure (ret, (begin, end))
{-# INLINE withSpan #-}

line :: Parser ()
line = do
  l <- getLine
  rl <- getRefLine
  when (l /= rl) $ failWith $ EIndentOverLine rl l
{-# INLINE line #-}

saveRef :: MonadParser p => p a -> p a
saveRef p = do
  r <- getRefPos
  getPos >>= setRefPos
  x <- p
  setRefPos r
  pure x
{-# INLINE saveRef #-}

align :: Parser ()
align = do
  c <- getColumn
  rc <- getRefColumn
  when (c /= rc) $ failWith $ EIndentNotAligned rc c
{-# INLINE align #-}

indented :: Parser ()
indented = do
  c <- getColumn
  rc <- getRefColumn
  if c <= rc then
    failWith $ ENotEnoughIndent rc c
  else
    getLine >>= setRefLine
{-# INLINE indented #-}

linefold :: Parser ()
linefold = line <|> indented
{-# INLINE linefold #-}

notByte :: Word8 -> Parser Word8
notByte b = byteSatisfy (/= b) <?> "not " <> showByte b
{-# INLINE notByte #-}

anyByte :: Parser Word8
anyByte = byteSatisfy (const True)
{-# INLINE anyByte #-}

asciiByte :: Parser Word8
asciiByte = byteSatisfy (< 128)
{-# INLINE asciiByte #-}

digitByte :: Int -> Parser Word8
digitByte base = byteSatisfy (isDigit base)
{-# INLINE digitByte #-}

integer :: (Num a, MonadParser p) => p sep -> Int -> p (a, Int)
integer sep base = label (integerLabel base) $ do
  d <- digit base
  accum 1 $ fromIntegral d
  where accum !i !n = next i n <|> pure (n, i)
        next !i !n = do
          d <- sep *> digit base
          accum (i + 1) $ n * fromIntegral base + fromIntegral d
{-# INLINE integer #-}

integerLabel :: Int -> String
integerLabel 2  = "binary integer"
integerLabel 8  = "octal integer"
integerLabel 10 = "decimal integer"
integerLabel 16 = "hexadecimal integer"
integerLabel b  = "integer of base " <> show b

digitToInt :: Int -> Word8 -> Word
digitToInt base b
  | n <- (fromIntegral b :: Word) - fromIntegral asc_0, base <= 10 || n <= 9  = n
  | n <- (fromIntegral b :: Word) - fromIntegral asc_A, n               <= 26 = n + 10
  | n <- (fromIntegral b :: Word) - fromIntegral asc_a                        = n + 10
{-# INLINE digitToInt #-}

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

signed :: (Num a, MonadParser p) => p a -> p a
signed p = ($) <$> ((id <$ byte asc_plus) <|> (negate <$ byte asc_minus) <|> pure id) <*> p
{-# INLINE signed #-}

fraction :: (Num a, MonadParser p) => p expSep -> Int -> Int -> p digitSep -> p (a, Int, a)
fraction expSep expBase coeffBasePow digitSep = do
  let coeffBase = expBase ^ coeffBasePow
  coeff <- fst <$> integer digitSep coeffBase
  (frac, fracLen) <- option (0, 0) $ byte asc_point *> integer digitSep coeffBase
  expVal <- option 0 $ expSep *> signed (fst <$> integer digitSep 10)
  pure (coeff * fromIntegral coeffBase ^ fracLen + frac,
        expBase,
        expVal - fromIntegral (fracLen * coeffBasePow))
{-# INLINE fraction #-}

fractionDec :: (Num a, MonadParser p) => p digitSep -> p (a, Int, a)
fractionDec sep = fraction (byteSatisfy (\b -> b == asc_E || b == asc_e)) 10 1 sep <?> "fraction"
{-# INLINE fractionDec #-}

fractionHex :: (Num a, MonadParser p) => p digitSep -> p (a, Int, a)
fractionHex sep = fraction (byteSatisfy (\b -> b == asc_P || b == asc_p)) 2 4 sep <?> "hexadecimal fraction"
{-# INLINE fractionHex #-}

char' :: Char -> Parser Char
char' x =
  let l = C.toLower x
      u = C.toUpper x
  in satisfy (\c -> c == l || c == u)
{-# INLINE char' #-}

notChar :: Char -> Parser Char
notChar c = satisfy (/= c)
{-# INLINE notChar #-}

anyChar :: Parser Char
anyChar = satisfy (const True)
{-# INLINE anyChar #-}

alphaNumChar :: Parser Char
alphaNumChar = satisfy C.isAlphaNum <?> "alphanumeric character"
{-# INLINE alphaNumChar #-}

letterChar :: Parser Char
letterChar = satisfy C.isLetter <?> "letter"
{-# INLINE letterChar #-}

lowerChar :: Parser Char
lowerChar = satisfy C.isLower <?> "lowercase letter"
{-# INLINE lowerChar #-}

upperChar :: Parser Char
upperChar = satisfy C.isUpper <?> "uppercase letter"
{-# INLINE upperChar #-}

spaceChar :: Parser Char
spaceChar = satisfy C.isSpace <?> "space"
{-# INLINE spaceChar #-}

symbolChar :: Parser Char
symbolChar = satisfy C.isSymbol <?> "symbol"
{-# INLINE symbolChar #-}

punctuationChar :: Parser Char
punctuationChar = satisfy C.isPunctuation <?> "punctuation"
{-# INLINE punctuationChar #-}

digitChar :: Int -> Parser Char
digitChar base = unsafeAsciiToChar <$> digitByte base
{-# INLINE digitChar #-}

asciiChar :: Int -> Parser Char
asciiChar base = unsafeAsciiToChar <$> digitByte base
{-# INLINE asciiChar #-}

categoryChar :: C.GeneralCategory -> Parser Char
categoryChar cat = satisfy ((== cat) . C.generalCategory) <?> untitle (show cat)
{-# INLINE categoryChar #-}

string :: Text -> Parser Text
string t = t <$ bytes (T.encodeUtf8 t)
{-# INLINE string #-}

string' :: Text -> Parser Text
string' t =
  let tl = T.toLower t
      name = "case-insensitive " <> T.unpack tl
  in label name $ do
    t' <- asString (skipCount (T.length tl) anyChar)
    when (T.toLower t' /= tl) $ failWith $ EExpected [name]
    pure t'
{-# INLINE string' #-}

asString :: MonadParser p => p () -> p Text
asString p = T.decodeUtf8 <$> asBytes p
{-# INLINE asString #-}

takeString :: Int -> Parser Text
takeString n = asString (skipCount n anyChar) <?> "string of length " <> show n
{-# INLINE takeString #-}

untitle :: String -> String
untitle []     = []
untitle (x:xs) = C.toLower x : go xs
  where go [] = ""
        go (y:ys) | C.isUpper y = ' ' : C.toLower y : untitle ys
                  | otherwise   = y : ys
