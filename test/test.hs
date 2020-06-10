{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}
module Main (main) where

import Control.Monad (replicateM, replicateM_)
import Data.ByteString (ByteString)
import Data.Either (isLeft)
import Data.Text (Text)
import Prelude hiding (getLine)
import System.Random
import Test.Tasty
import Test.Tasty.HUnit
import Text.PariPari
import Text.PariPari.Internal.Chunk (stringToChunk, asc_a, asc_0, asc_9)
import qualified Data.Char as C
import qualified Data.List.NonEmpty as NE
import Data.String (IsString(..))

runAcceptor' :: Chunk k => Acceptor k a -> FilePath -> k -> Either Error a
runAcceptor' a f k = maybe (Left $ EFail "Nothing") Right $ runAcceptor a f k

main :: IO ()
main = defaultMain tests

randomTries :: Int
randomTries = 1000

randomStringLen :: Int
randomStringLen = 1000

runReporterEither :: Chunk k => Reporter k a -> FilePath -> k -> Either [Report] a
runReporterEither p f k = case runReporter p f k of
  (Just a, []) -> Right a
  (_,      r)  -> Left r

-- Only generate valid Unicode characters
randomChar :: IO Char
randomChar = do
  c <- randomIO
  let n = C.ord c
  if n == 0 || (n >= 0xD800 && n <= 0xDFFF) || n > 0x10FFFF then
    randomChar
  else
    pure c

randomString :: IO String
randomString = do
  n <- randomRIO (1, randomStringLen)
  replicateM n randomChar

randomAsciiString :: IO String
randomAsciiString = do
  n <- randomRIO (1, randomStringLen)
  replicateM n (randomRIO (C.chr 1, C.chr 127))

tests :: TestTree
tests = testGroup "Tests"
  [ testGroup "Acceptor"
    [ testGroup "Text"       $ parserTests @Text       runAcceptor'
    , testGroup "ByteString" $ parserTests @ByteString runAcceptor'
    ]

  , testGroup "Reporter"
    [ testGroup "Text"       $ parserTests @Text       runReporterEither
    , testGroup "ByteString" $ parserTests @ByteString runReporterEither
    ]

  , testGroup "Reporter specific" reporterTests
  ]

parserTests :: forall k p e. (Parser k p, Chunk k, IsString k, Eq e, Show e, Show k)
          => (forall a. p a -> FilePath -> k -> Either e a) -> [TestTree]
parserTests run =
  [ testGroup "ChunkParser"
    [ testCase "getFile" $ do
        ok getFile "" "filename"

    , testCase "getPos" $ do
        ok getPos "" (Pos 1 1)
        ok (char 'a' *> getPos) "abc" (Pos 1 2)
        ok (char 'a' *> char '\n' *> getPos) "a\nb" (Pos 2 1)
        ok (chunk "a\n" *> getPos) "a\nb" (Pos 1 3) -- chunk must not contain newlines!
        ok (char 'a' *> char '\n' *> char 'b' *> getPos) "a\nb" (Pos 2 2)

    , testCase "getRefPos" $ do
        ok getRefPos "" (Pos 1 1)
        ok (char 'a' *> getRefPos) "abc" (Pos 1 1)
        ok (char 'a' *> char '\n' *> withRefPos getRefPos) "a\nb" (Pos 2 1)
        ok (char 'a' *> char '\n' *> char 'b' *> withRefPos getRefPos) "a\nb" (Pos 2 2)
        ok (char 'a' *> char '\n' *> withRefPos (char 'b' *> getRefPos)) "a\nb" (Pos 2 1)

    , testCase "notFollowedBy" $ do
        ok (char 'a' <* notFollowedBy (char 'c')) "abc" 'a'
        err (char 'a' <* notFollowedBy (char 'b')) "abc"
        ok (char 'a' *> notFollowedBy (chunk "bd") *> char 'b') "abc" 'b'
        err (char 'a' *> notFollowedBy (chunk "bc")) "abc"
        ok (char 'a' *> notFollowedBy (char 'c') *> getPos) "abc" (Pos 1 2)

    , testCase "lookAhead" $ do
        ok (lookAhead (char 'a')) "abc" 'a'
        ok (lookAhead (char 'a') *> getPos) "abc" (Pos 1 1)
        ok (lookAhead (chunk "ab") *> getPos) "abc" (Pos 1 1)
        err (lookAhead (char 'b')) "abc"
        err (lookAhead (chunk "bd")) "abc"
        err (lookAhead (char 'a')) ""

    , testCase "failWith" $
        err (failWith (EFail "empty") :: p ()) "abc"

    , testCase "eof" $ do
        ok eof "" ()
        ok eof "\0" ()
        ok eof "\0\0" ()
        ok (chunk "abc" *> eof) "abc" ()
        ok (chunk "abc" *> eof) "abc\0" ()
        err eof "abc"
        err (chunk "ab" *> eof) "abc"

    , testCase "label" $ do
        ok (label "blub" $ char 'a') "abc" 'a'
        err (label "blub" $ char 'b') "abc"
        ok (char 'a' <?> "blub") "abc" 'a'

    , testCase "hidden" $ do
        ok (hidden $ char 'a') "abc" 'a'
        err (hidden $ char 'b') "abc"

    , testCase "try" $ do
        ok (try $ char 'a') "abc" 'a'
        err (try $ char 'b') "abc"

    , testCase "(<!>)" $ do
        ok (char 'b' <!> char 'a' <!> char 'c') "abc" 'a'
        ok (chunk "abd" <!> chunk "abc" <!> chunk "abe") "abcdef" "abc"

    , testCase "recover" $ do
        ok (recover (char 'a' <* eof) (char 'b')) "a" 'a'
        err (recover (char 'a') (char 'b')) "c"
        err (recover (char 'a' <* eof) (char 'b')) "c"

    , testCase "chunk" $ do
        ok (chunk "ab") "abc" "ab"
        err (chunk "bc") "abc"
        err (chunk "ab") ""

    , testCase "asChunk" $ do
        ok (asChunk (void $ chunk "ab")) "abc" "ab"
        ok (asChunk (void $ anyChar *> anyChar)) "abc" "ab"
        ok (asChunk (skipCount 2 anyChar)) "abc" "ab"
        err (asChunk (void $ chunk "bc")) "abc"
        err (asChunk (void $ chunk "ab")) ""

    , testCase "char" $ do
        ok (char 'a') "abc" 'a'
        ok (char 'a' <* eof) "a" 'a'
        err (char 'b') "abc"
        err (char 'a') ""

    , testCase "char-random" $ replicateM_ randomTries $ do
        s <- randomString
        ok (traverse char s *> eof) s ()

    , testCase "scan" $ do
        ok (scan (\c -> if c == 'a' then Just c else Nothing)) "abc" 'a'
        ok (scan (\c -> if c == 'a' then Just c else Nothing) <* eof) "a" 'a'
        err (scan (\c -> if c == 'b' then Just c else Nothing)) "abc"
        err (scan (\c -> if c == 'a' then Just c else Nothing)) ""

        -- because of sentinel
        err (scan (\c -> if c == '\0' then Just c else Nothing)) "\0"
        err (scan (\c -> if c == '\0' then Just c else Nothing)) ""

    , testCase "asciiScan" $ do
        ok (asciiScan (\c -> if c == asc_a then Just c else Nothing)) "abc" asc_a
        ok (asciiScan (\c -> if c == asc_a then Just c else Nothing) <* eof) "a" asc_a
        err (asciiScan (\c -> if c == asc_0 then Just c else Nothing)) "abc"
        err (asciiScan (\c -> if c == asc_0 then Just c else Nothing)) ""

        -- because of sentinel
        err (asciiScan (\c -> if c == 0 then Just c else Nothing)) "\0"
        err (asciiScan (\c -> if c == 0 then Just c else Nothing)) ""

    , testCase "asciiByte" $ do
        ok (asciiByte asc_a) "abc" asc_a
        ok (asciiByte asc_a <* eof) "a" asc_a
        ok (asciiByte 127 <* eof) "\x7F" 127
        err (asciiByte asc_0) "abc"
        err (asciiByte asc_0) ""

    , testCase "asciiByte-random" $ replicateM_ randomTries $ do
        s <- randomAsciiString
        ok (traverse (asciiByte . fromIntegral . C.ord) s *> eof) s ()
    ]

  , testGroup "Alternative"
    [ testCase "(<|>)" $ do
        ok (char 'b' <|> char 'a' <|> char 'c') "abc" 'a'
        ok (chunk "abd" <|> chunk "abc" <|> chunk "abe") "abcdef" "abc"

    , testCase "empty" $ do
        err (empty :: p ()) "abc"
        err (empty :: p ()) ""
    ]

  , testGroup "MonadFail"
    [ testCase "fail" $ do
        err (failWith (EFail "empty") :: p ()) "abc"
    ]

  , testGroup "Basic Combinators"
    [ testCase "optional" $ do
        ok (optional (char 'a')) "abc" (Just 'a')
        ok (optional (char 'b')) "abc" Nothing
        ok (optional (char 'a')) "" Nothing

    , testCase "some" $ do
        ok (some (char 'a')) "abc" (NE.fromList "a")
        ok (some (char 'a')) "aabc" (NE.fromList "aa")
        err (some (char 'b')) "abc"
        err (some (char 'a')) ""

    , testCase "many" $ do
        ok (many (char 'a')) "abc" "a"
        ok (many (char 'a')) "aabc" "aa"
        ok (many (char 'b')) "abc" ""
        ok (many (char 'a')) "" ""
    ]

  , testGroup "Position Combinators"
    [ testCase "getLine" $ do
        ok getLine "" 1
        ok (char 'a' *> getLine) "abc" 1
        ok (char 'a' *> char '\n' *> getLine) "a\nb" 2
        ok (char 'a' *> char '\n' *> char 'b' *> getLine) "a\nb" 2

    , testCase "getRefLine" $ do
        ok getRefLine "" 1
        ok (char 'a' *> getRefLine) "abc" 1
        ok (char 'a' *> char '\n' *> withRefPos getRefLine) "a\nb" 2
        ok (char 'a' *> char '\n' *> char 'b' *> withRefPos getRefLine) "a\nb" 2
        ok (char 'a' *> char '\n' *> withRefPos (char 'b' *> getRefLine)) "a\nb" 2

    , testCase "getCol" $ do
        ok getCol "" 1
        ok (char 'a' *> getCol) "abc" 2
        ok (char 'a' *> char '\n' *> getCol) "a\nb" 1
        ok (char 'a' *> char '\n' *> char 'b' *> getCol) "a\nb" 2

    , testCase "getRefCol" $ do
        ok getRefCol "" 1
        ok (char 'a' *> getRefCol) "abc" 1
        ok (char 'a' *> char '\n' *> withRefPos getRefCol) "a\nb" 1
        ok (char 'a' *> char '\n' *> char 'b' *> withRefPos getRefCol) "a\nb" 2
        ok (char 'a' *> char '\n' *> withRefPos (char 'b' *> getRefCol)) "a\nb" 1

    , testCase "withPos" $ do
        ok (withPos $ char 'a') "abc" (Pos 1 1, 'a')
        ok (char 'a' *> withPos (char 'b')) "abc" (Pos 1 2, 'b')
        ok (char 'a' *> char '\n' *> withPos (char 'b')) "a\nb" (Pos 2 1, 'b')

    , testCase "withSpan" $ do
        ok (withSpan $ chunk "ab") "abc" (Pos 1 1, Pos 1 3, "ab")
        ok (char 'a' *> withSpan (chunk "bcd")) "abcde" (Pos 1 2, Pos 1 5, "bcd")
        ok (char 'a' *> char '\n' *> withSpan (chunk "bcd")) "a\nbcde" (Pos 2 1, Pos 2 4, "bcd")
    ]

  , testGroup "Char Combinators"
    [ testCase "satisfy" $ do
        ok (satisfy (== 'a')) "abc" 'a'
        ok (satisfy (== 'a') <* eof) "a" 'a'
        err (satisfy (== 'b')) "abc"
        err (satisfy (== 'a')) ""

        -- because of sentinel
        err (satisfy (== '\0')) "\0"
        err (satisfy (== '\0')) ""

    , testCase "satisfy-random" $ replicateM_ randomTries $ do
        s <- randomString
        ok (traverse (satisfy . (==)) s *> eof) s ()

    , testCase "asciiSatisfy" $ do
        ok (asciiSatisfy (== asc_a)) "abc" asc_a
        ok (asciiSatisfy (== asc_a) <* eof) "a" asc_a
        err (asciiSatisfy (== asc_0)) "abc"
        err (asciiSatisfy (== asc_0)) ""

        -- because of sentinel
        err (asciiSatisfy (== 0)) "\0"
        err (asciiSatisfy (== 0)) ""

    , testCase "asciiSatisfy-random" $ replicateM_ randomTries $ do
        s <- randomAsciiString
        ok (traverse (asciiSatisfy . (==) . fromIntegral . C.ord) s *> eof) s ()

    , testCase "string" $ do
        ok (string "ab") "abc" (stringToChunk "ab")
        err (string "bc") "abc"
        err (string "ab") ""

    , testCase "string-random" $ replicateM_ randomTries $ do
        s <- randomString
        ok (string s <* eof) s (stringToChunk s)

    , testCase "anyChar" $ do
        ok anyChar "abc" 'a'
        ok (anyChar <* eof) "a" 'a'
        err anyChar ""

    , testCase "anyChar-random" $ replicateM_ randomTries $ do
        s <- randomString
        ok (traverse (const anyChar) s *> eof) s ()

    , testCase "anyAsciiByte" $ do
        ok anyAsciiByte "abc" asc_a
        ok (anyAsciiByte <* eof) "a" asc_a
        err anyAsciiByte ""
        err anyAsciiByte "\x80"

    , testCase "anyAsciiByte-random" $ replicateM_ randomTries $ do
        s <- randomAsciiString
        ok (traverse (const anyAsciiByte) s *> eof) s ()

    , testCase "notChar" $ do
        ok (notChar 'b') "abc" 'a'
        ok (notChar 'b' <* eof) "a" 'a'
        err (notChar 'a') "a"
        err (notChar 'a') ""

    , testCase "char'" $ do
        ok (char' 'a') "a" 'a'
        ok (char' 'a' <* eof) "a" 'a'
        ok (char' 'a') "A" 'A'
        ok (char' 'A') "a" 'a'
        ok (char' 'A') "A" 'A'
        ok (char' '9') "9" '9'
        err (char' 'a') "b"
        err (char' 'a') ""

    , testCase "alphaNumChar" $ do
        ok (alphaNumChar <* eof) "a" 'a'
        ok alphaNumChar "9" '9'
        err alphaNumChar "_"
        err alphaNumChar ""

    , testCase "digitChar" $ do
        ok (digitChar 10 <* eof) "9" '9'
        ok (digitChar 36) "z" 'z'
        err (digitChar 2) "2"
        err (digitChar 2) ""

    , testCase "letterChar" $ do
        ok (letterChar <* eof) "a" 'a'
        err letterChar "9"
        err letterChar "_"
        err letterChar ""

    , testCase "lowerChar" $ do
        ok (lowerChar <* eof) "a" 'a'
        err lowerChar "A"
        err lowerChar "9"
        err lowerChar "_"
        err lowerChar ""

    , testCase "upperChar" $ do
        ok (upperChar <* eof) "A" 'A'
        err upperChar "a"
        err upperChar "9"
        err upperChar "_"
        err upperChar ""

    , testCase "symbolChar" $ do
        ok (symbolChar <* eof) "+" '+'
        err symbolChar "a"
        err symbolChar "."
        err symbolChar ""

    , testCase "punctuationChar" $ do
        ok (punctuationChar <* eof) "." '.'
        err punctuationChar "+"
        err punctuationChar "a"
        err punctuationChar ""

    , testCase "spaceChar" $ do
        ok (spaceChar <* eof) " " ' '
        ok (spaceChar <* eof) "\n" '\n'
        ok (spaceChar <* eof) "\r" '\r'
        ok (spaceChar <* eof) "\t" '\t'
        err spaceChar "a"
        err spaceChar ""

    , testCase "asciiChar" $ do
        ok (asciiChar <* eof) "a" 'a'
        ok (asciiChar <* eof) "\x7F" '\x7F'
        err asciiChar "\x80"
        err asciiChar ""

    , testCase "categoryChar" $ do
        ok (categoryChar C.UppercaseLetter <* eof) "A" 'A'
        err (categoryChar C.UppercaseLetter) "a"
        err (categoryChar C.UppercaseLetter) ""

    , testCase "digitByte" $ do
        ok (digitByte 2 <* eof) "0" asc_0
        ok (digitByte 10 <* eof) "9" asc_9
        err (digitByte 10) "\0"
        err (digitByte 10) ""

    , testCase "skipChars" $ do
        ok (skipChars 0) "" ()
        ok (skipChars 3 <* eof) "abc" ()
        ok (skipChars 3 <* char 'b') "aaab" ()
        err (skipChars 1) ""
        err (skipChars 2) "a"

    , testCase "takeChars" $ do
        ok (takeChars 0) "" (stringToChunk "")
        ok (takeChars 3 <* eof) "abc" (stringToChunk "abc")
        ok (takeChars 3 <* char 'b') "aaab" (stringToChunk "aaa")
        err (takeChars 1) ""
        err (takeChars 2) "a"

    , testCase "skipCharsWhile" $ do
        ok (skipCharsWhile (== 'a')) "" ()
        ok (skipCharsWhile (== 'a')) "b" ()
        ok (skipCharsWhile (== 'a') *> eof) "aaa" ()
        ok (skipCharsWhile (== 'a') *> char 'b') "aaab" 'b'

    , testCase "takeCharsWhile" $ do
        ok (takeCharsWhile (== 'a')) "" (stringToChunk "")
        ok (takeCharsWhile (== 'a')) "b" (stringToChunk "")
        ok (takeCharsWhile (== 'a') <* eof) "aaa" (stringToChunk "aaa")
        ok (takeCharsWhile (== 'a') <* char 'b') "aaab" (stringToChunk "aaa")

    , testCase "skipCharsWhile1" $ do
        err (skipCharsWhile1 (== 'a')) ""
        err (skipCharsWhile1 (== 'a')) "b"
        ok (skipCharsWhile1 (== 'a') *> eof) "aaa" ()
        ok (skipCharsWhile1 (== 'a') *> char 'b') "aaab" 'b'

    , testCase "takeCharsWhile1" $ do
        err (takeCharsWhile1 (== 'a')) ""
        err (takeCharsWhile1 (== 'a')) "b"
        ok (takeCharsWhile1 (== 'a') <* eof) "aaa" (stringToChunk "aaa")
        ok (takeCharsWhile1 (== 'a') <* char 'b') "aaab" (stringToChunk "aaa")
    ]

  , testGroup "Fraction Combinators"
    [ testCase "fractionDec" $ do
        okFraction (fractionDec (pure ()) <* eof) "1.23" (Right (123, 10, -2))
        okFraction (fractionDec (pure ()) <* eof) "99e0" (Right (99, 10, 0))
        okFraction (fractionDec (pure ()) <* eof) "123.45" (Right (12345, 10, -2))
        okFraction (fractionDec (pure ()) <* eof) "00123." (Right (123, 10, 0))
        okFraction (fractionDec (pure ()) <* eof) "456.000" (Right (456000, 10, -3))

        okFraction (fractionDec (pure ()) <* eof) "987e-5" (Right (987, 10, -5))
        okFraction (fractionDec (pure ()) <* eof) "987.e-123" (Right (987, 10, -123))
        okFraction (fractionDec (pure ()) <* eof) "987.654e-67" (Right (987654, 10, -70))
        okFraction (fractionDec (pure ()) <* eof) "987.654000e-7" (Right (987654000, 10, -13))
        okFraction (fractionDec (pure ()) <* eof) "000987.654000e-7" (Right (987654000, 10, -13))

        okFraction (fractionDec (pure ()) <* eof) "987e+5" (Right (987, 10, 5))
        okFraction (fractionDec (pure ()) <* eof) "987.e+123" (Right (987, 10, 123))
        okFraction (fractionDec (pure ()) <* eof) "987.654e+67" (Right (987654, 10, 64))
        okFraction (fractionDec (pure ()) <* eof) "987.654000e+7" (Right (987654000, 10, 1))
        okFraction (fractionDec (pure ()) <* eof) "000987.654000e+7" (Right (987654000, 10, 1))

        okFraction (fractionDec (pure ()) <* eof) "987e5" (Right (987, 10, 5))
        okFraction (fractionDec (pure ()) <* eof) "987.e123" (Right (987, 10, 123))
        okFraction (fractionDec (pure ()) <* eof) "987.654e67" (Right (987654, 10, 64))
        okFraction (fractionDec (pure ()) <* eof) "987.654000e7" (Right (987654000, 10, 1))
        okFraction (fractionDec (pure ()) <* eof) "000987.654000e7" (Right (987654000, 10, 1))

        okFraction (fractionDec (pure ())) "123" (Left 123)

        errFraction (fractionDec (pure ())) "123e"
        errFraction (fractionDec (pure ())) ""
        errFraction (fractionDec (pure ())) "abc"
    ]

  , testGroup "Integer Combinators"
    [ testCase "decimal" $ do
        ok @Integer (decimal <* eof) "0123" 123
        ok @Integer (decimal <* eof) "1234567890" 1234567890
        ok @Integer (decimal <* string "abc" <* eof) "123abc" 123
        err @Integer decimal "abc"
        err @Integer decimal "-1"
        err @Integer decimal ""

    , testCase "octal" $ do
        ok @Integer (octal <* eof) "0123" 0o123
        ok @Integer (octal <* eof) "12345670" 0o12345670
        ok @Integer (octal <* string "abc" <* eof) "123abc" 0o123
        err @Integer octal "8abc"
        err @Integer octal "-1"
        err @Integer octal ""

    , testCase "hexadecimal" $ do
        ok @Integer (hexadecimal <* eof) "0123" 0x123
        ok @Integer (hexadecimal <* eof) "123456789aBcDeF0" 0x123456789ABCDEF0
        ok @Integer (hexadecimal <* string "xyz" <* eof) "123xyz" 0x123
        err @Integer hexadecimal "gabc"
        err @Integer hexadecimal "-1"
        err @Integer hexadecimal ""

    , testCase "integer" $ do
        ok @Integer (integer (char '_') 10) "1_2_3" 123
        ok @Integer (integer (char '_') 10 <* char '_') "1_2_3_" 123
        ok @Integer (integer (optional $ char '_') 10) "123_456_789" 123456789
        err @Integer (integer (pure ()) 10) "abc"
        err @Integer (integer (char '_') 10) "_1_2_3"
        err @Integer (integer (pure ()) 10) "-1"
        err @Integer (integer (pure ()) 10) ""

        ok @Integer (integer (pure ()) 2) "101" 5
        ok @Integer (integer (pure ()) 7) "321" 162
        ok @Integer (integer (pure ()) 36) "XyZ" 44027

    , testCase "integer'" $ do
        ok @(Integer, Int) (integer' (char '_') 10) "1_2_3" (123, 3)
        ok @(Integer, Int) (integer' (char '_') 10 <* char '_') "1_2_3_" (123, 3)
        ok @(Integer, Int) (integer' (optional $ char '_') 10) "123_456_789" (123456789, 9)
        err @(Integer, Int) (integer' (pure ()) 10) "abc"
        err @(Integer, Int) (integer' (char '_') 10) "_1_2_3"
        err @(Integer, Int) (integer' (pure ()) 10) "-1"
        err @(Integer, Int) (integer' (pure ()) 10) ""

        ok @(Integer, Int) (integer' (pure ()) 10) "0123" (123, 4)
        ok @(Integer, Int) (integer' (pure ()) 10) "01230" (1230, 5)
        ok @(Integer, Int) (integer' (pure ()) 10) "000" (0, 3)

        ok @(Integer, Int) (integer' (pure ()) 2) "101" (5, 3)
        ok @(Integer, Int) (integer' (pure ()) 7) "321" (162, 3)
        ok @(Integer, Int) (integer' (pure ()) 36) "XyZ" (44027, 3)

    , testCase "signed" $ do
        ok @Integer (signed decimal) "-123" (-123)
        ok @Integer (signed decimal) "+123" 123
        err @Integer (signed decimal) "- 123"
        err @Integer (signed decimal) "+ 123"
        err @Integer (signed decimal) ""

    , testCase "digit" $ do
        ok (digit 2) "1" 1
        ok (digit 10) "7" 7
        ok (digit 36) "Z" 35
        err (digit 10) "a"
        err (digit 10) ""
    ]
  ]
  where
  ok :: (Eq a, Show a, HasCallStack) => p a -> String -> a -> Assertion
  ok p i o = run p "filename" (stringToChunk @k i) @?= Right o
  err :: (Eq a, Show a, HasCallStack) => p a -> String -> Assertion
  err p i = assertBool "err" $ isLeft $ run p "filename" (stringToChunk @k i)
  errFraction :: HasCallStack => p (Either Integer (Integer, Int, Integer)) -> String -> Assertion
  errFraction = err
  okFraction :: HasCallStack => p (Either Integer (Integer, Int, Integer)) -> String -> Either Integer (Integer, Int, Integer) -> Assertion
  okFraction = ok

reporterTests :: [TestTree]
reporterTests =
  [ testCase "recover" $ do
      run (char 'a' <* eof) "a" (Just 'a', 0)
      run (char 'a') "b" (Nothing, 1)
      run (recover (char 'a' <* eof) (char 'b')) "a" (Just 'a', 0)
      run (recover (char 'a') (char 'b')) "b" (Just 'b', 1)
      run ((,)
            <$> recover (char 'a') (char 'b')
            <*> recover (char 'a') (char 'c')) "bc" (Just ('b', 'c'), 2)
      run (recover (char 'a') (char 'b') *>
           recover (char 'a') (char 'c') *>
           char 'a') "bcd" (Nothing, 3)
      run (recover (char 'a' <* eof) (char 'b')) "c" (Nothing, 1)
  ]
  where
  run :: (Eq a, Show a, HasCallStack) => Reporter Text a -> Text -> (Maybe a, Int) -> Assertion
  run p i (o, r) = do
    let (out, rep) = runReporter p "filename" i
    out @?= o
    length rep @?= r

{-
TODO:

Instances:
Semigroup
Monoid
Functor
Applicative
Monad
MonadPlus
Alternative

Additional combinators provided by parser-combinators.
They are also tested by megaparsec:
endBy1
someTill
sepBy1
sepEndBy1
between
choice
count
count'
eitherP
endBy
manyTill
option
sepBy
sepEndBy
skipMany
skipSome
skipCount
skipManyTill
skipSomeTill

Identation:
align
indented
line
linefold

Fraction:
fraction
fractionHex

Char Combinators:
scanChars
scanChars1
-}
