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
import GHC.Stack (HasCallStack)
import Prelude hiding (getLine)
import System.Random
import Test.Tasty
import Test.Tasty.HUnit
import Text.PariPari
import Text.PariPari.Internal.Chunk (stringToChunk, asc_a, asc_0, asc_9)
import qualified Data.Char as C
import qualified Data.List.NonEmpty as NE

main :: IO ()
main = defaultMain tests

randomTries :: Int
randomTries = 1000

randomStringLen :: Int
randomStringLen = 1000

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
  [ testGroup "Chunk"
    [ testGroup "Acceptor" $ chunkTests runAcceptor
    , testGroup "Reporter" $ chunkTests runAcceptor
    ]

  , testGroup "Char"
    [ testGroup "Acceptor"
      [ testGroup "Text"       $ charTests @Text       runAcceptor
      , testGroup "ByteString" $ charTests @ByteString runAcceptor
      ]

    , testGroup "Reporter"
      [ testGroup "Text"       $ charTests @Text       runReporter
      , testGroup "ByteString" $ charTests @ByteString runReporter
      ]
    ]
  ]

charTests :: forall k p e. (CharParser k p, CharChunk k, Eq e, Show e, Show k)
          => (forall a. p a -> FilePath -> k -> Either e a) -> [TestTree]
charTests run =
  [ testGroup "CharParser" $
    [ testCase "char" $ do
        ok (char 'a') "abc" 'a'
        ok (char 'a' <* eof) "a" 'a'
        err (char 'b') "abc"
        err (char 'a') ""

    , testCase "char-random" $ replicateM_ randomTries $ do
        s <- randomString
        ok (traverse char s *> eof) s ()

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

chunkTests :: forall p e. (ChunkParser Text p, Eq e, Show e)
          => (forall a. p a -> FilePath -> Text -> Either e a) -> [TestTree]
chunkTests run =
  [ testGroup "ChunkParser"
    [ testCase "getFile" $ do
        ok getFile "" "filename"

    , testCase "getPos" $ do
        ok getPos "" (Pos 1 1)
        ok (element 'a' *> getPos) "abc" (Pos 1 2)
        ok (element 'a' *> element '\n' *> getPos) "a\nb" (Pos 2 1)
        ok (chunk "a\n" *> getPos) "a\nb" (Pos 1 3) -- chunk must not contain newlines!
        ok (element 'a' *> element '\n' *> element 'b' *> getPos) "a\nb" (Pos 2 2)

    , testCase "getRefPos" $ do
        ok getRefPos "" (Pos 1 1)
        ok (element 'a' *> getRefPos) "abc" (Pos 1 1)
        ok (element 'a' *> element '\n' *> withRefPos getRefPos) "a\nb" (Pos 2 1)
        ok (element 'a' *> element '\n' *> element 'b' *> withRefPos getRefPos) "a\nb" (Pos 2 2)
        ok (element 'a' *> element '\n' *> withRefPos (element 'b' *> getRefPos)) "a\nb" (Pos 2 1)

    , testCase "notFollowedBy" $ do
        ok (element 'a' <* notFollowedBy (element 'c')) "abc" 'a'
        err (element 'a' <* notFollowedBy (element 'b')) "abc"
        ok (element 'a' *> notFollowedBy (chunk "bd") *> element 'b') "abc" 'b'
        err (element 'a' *> notFollowedBy (chunk "bc")) "abc"
        ok (element 'a' *> notFollowedBy (element 'c') *> getPos) "abc" (Pos 1 2)

    , testCase "lookAhead" $ do
        ok (lookAhead (element 'a')) "abc" 'a'
        ok (lookAhead (element 'a') *> getPos) "abc" (Pos 1 1)
        ok (lookAhead (chunk "ab") *> getPos) "abc" (Pos 1 1)
        err (lookAhead (element 'b')) "abc"
        err (lookAhead (chunk "bd")) "abc"
        err (lookAhead (element 'a')) ""

    , testCase "failWith" $
        err (failWith (ECombinator "empty") :: p ()) "abc"

    , testCase "eof" $ do
        ok eof "" ()
        ok (chunk "abc" *> eof) "abc" ()
        err eof "abc"
        err eof "\0"
        err (chunk "ab" *> eof) "abc"

    , testCase "label" $ do
        ok (label "blub" $ element 'a') "abc" 'a'
        err (label "blub" $ element 'b') "abc"
        ok (element 'a' <?> "blub") "abc" 'a'

    , testCase "hidden" $ do
        ok (hidden $ element 'a') "abc" 'a'
        err (hidden $ element 'b') "abc"

    , testCase "commit" $ do
        ok (commit $ element 'a') "abc" 'a'
        err (commit $ element 'b') "abc"

    , testCase "element" $ do
        ok (element 'a') "abc" 'a'
        ok (element 'a' <* eof) "a" 'a'
        err (element 'b') "abc"
        err (element 'a') ""

        -- sentinel
        ok (element '\0') "\0" '\0'
        ok (element '\0' <* eof) "\0" '\0'
        err (element '\0') ""

    , testCase "chunk" $ do
        ok (chunk "ab") "abc" "ab"
        err (chunk "bc") "abc"
        err (chunk "ab") ""

    , testCase "asChunk" $ do
        ok (asChunk (void $ chunk "ab")) "abc" "ab"
        ok (asChunk (void $ anyElement *> anyElement)) "abc" "ab"
        ok (asChunk (skipCount 2 anyElement)) "abc" "ab"
        err (asChunk (void $ chunk "bc")) "abc"
        err (asChunk (void $ chunk "ab")) ""
    ]

  , testGroup "Position Combinators"
    [ testCase "getLine" $ do
        ok getLine "" 1
        ok (element 'a' *> getLine) "abc" 1
        ok (element 'a' *> element '\n' *> getLine) "a\nb" 2
        ok (element 'a' *> element '\n' *> element 'b' *> getLine) "a\nb" 2

    , testCase "getRefLine" $ do
        ok getRefLine "" 1
        ok (element 'a' *> getRefLine) "abc" 1
        ok (element 'a' *> element '\n' *> withRefPos getRefLine) "a\nb" 2
        ok (element 'a' *> element '\n' *> element 'b' *> withRefPos getRefLine) "a\nb" 2
        ok (element 'a' *> element '\n' *> withRefPos (element 'b' *> getRefLine)) "a\nb" 2

    , testCase "getColumn" $ do
        ok getColumn "" 1
        ok (element 'a' *> getColumn) "abc" 2
        ok (element 'a' *> element '\n' *> getColumn) "a\nb" 1
        ok (element 'a' *> element '\n' *> element 'b' *> getColumn) "a\nb" 2

    , testCase "getRefColumn" $ do
        ok getRefColumn "" 1
        ok (element 'a' *> getRefColumn) "abc" 1
        ok (element 'a' *> element '\n' *> withRefPos getRefColumn) "a\nb" 1
        ok (element 'a' *> element '\n' *> element 'b' *> withRefPos getRefColumn) "a\nb" 2
        ok (element 'a' *> element '\n' *> withRefPos (element 'b' *> getRefColumn)) "a\nb" 1

    , testCase "withPos" $ do
        ok (withPos $ element 'a') "abc" (Pos 1 1, 'a')
        ok (element 'a' *> withPos (element 'b')) "abc" (Pos 1 2, 'b')
        ok (element 'a' *> element '\n' *> withPos (element 'b')) "a\nb" (Pos 2 1, 'b')

    , testCase "withSpan" $ do
        ok (withSpan $ chunk "ab") "abc" ((Pos 1 1, Pos 1 3), "ab")
        ok (element 'a' *> withSpan (chunk "bcd")) "abcde" ((Pos 1 2, Pos 1 5), "bcd")
        ok (element 'a' *> element '\n' *> withSpan (chunk "bcd")) "a\nbcde" ((Pos 2 1, Pos 2 4), "bcd")
    ]

  , testGroup "Element Combinators"
    [ testCase "elementSatisfy" $ do
        ok (elementSatisfy (== 'a')) "abc" 'a'
        ok (elementSatisfy (== 'a') <* eof) "a" 'a'
        err (elementSatisfy (== 'b')) "abc"
        err (elementSatisfy (== 'a')) ""

        -- sentinel
        ok (elementSatisfy (== '\0')) "\0" '\0'
        ok (elementSatisfy (== '\0') <* eof) "\0" '\0'
        err (elementSatisfy (== '\0')) ""

    , testCase "anyElement" $ do
        ok anyElement "abc" 'a'
        err anyElement ""

    , testCase "notElement" $ do
        ok (notElement 'b') "abc" 'a'
        err (notElement 'a') "a"
        err (notElement 'a') ""

    , testCase "takeElementsWhile" $ do
        ok (takeElementsWhile (== 'a')) "" ""
        ok (takeElementsWhile (== 'a')) "b" ""
        ok (takeElementsWhile (== 'a') <* eof) "aaa" "aaa"
        ok (takeElementsWhile (== 'a') <* element 'b') "aaab" "aaa"

    , testCase "skipElements" $ do
        ok (skipElements 0) "" ()
        ok (skipElements 3 <* eof) "abc" ()
        ok (skipElements 3 <* element 'b') "aaab" ()
        err (skipElements 1) ""
        err (skipElements 2) "a"

    , testCase "takeElements" $ do
        ok (takeElements 0) "" ""
        ok (takeElements 3 <* eof) "abc" "abc"
        ok (takeElements 3 <* element 'b') "aaab" "aaa"
        err (takeElements 1) ""
        err (takeElements 2) "a"

    , testCase "skipElementsWhile" $ do
        ok (skipElementsWhile (== 'a')) "" ()
        ok (skipElementsWhile (== 'a')) "b" ()
        ok (skipElementsWhile (== 'a') *> eof) "aaa" ()
        ok (skipElementsWhile (== 'a') *> element 'b') "aaab" 'b'

    , testCase "skipElementsWhile1" $ do
        err (skipElementsWhile1 (== 'a')) ""
        err (skipElementsWhile1 (== 'a')) "b"
        ok (skipElementsWhile1 (== 'a') *> eof) "aaa" ()
        ok (skipElementsWhile1 (== 'a') *> element 'b') "aaab" 'b'

    , testCase "takeElementsWhile1" $ do
        err (takeElementsWhile1 (== 'a')) ""
        err (takeElementsWhile1 (== 'a')) "b"
        ok (takeElementsWhile1 (== 'a') <* eof) "aaa" "aaa"
        ok (takeElementsWhile1 (== 'a') <* element 'b') "aaab" "aaa"
    ]

  , testGroup "MonadFail"
    [ testCase "fail" $ do
        err (failWith (ECombinator "empty") :: p ()) "abc"
    ]

  , testGroup "Alternative"
    [ testCase "(<|>)" $ do
        ok (element 'b' <|> element 'a' <|> element 'c') "abc" 'a'
        ok (chunk "abd" <|> chunk "abc" <|> chunk "abe") "abcdef" "abc"

    , testCase "empty" $ do
        err (empty :: p ()) "abc"
        err (empty :: p ()) ""
    ]

  , testGroup "Basic Combinators"
    [ testCase "optional" $ do
        ok (optional (element 'a')) "abc" (Just 'a')
        ok (optional (element 'b')) "abc" Nothing
        ok (optional (element 'a')) "" Nothing

    , testCase "some" $ do
        ok (some (element 'a')) "abc" (NE.fromList "a")
        ok (some (element 'a')) "aabc" (NE.fromList "aa")
        err (some (element 'b')) "abc"
        err (some (element 'a')) ""

    , testCase "many" $ do
        ok (many (element 'a')) "abc" "a"
        ok (many (element 'a')) "aabc" "aa"
        ok (many (element 'b')) "abc" ""
        ok (many (element 'a')) "" ""
    ]
  ]
  where
  ok :: (Eq a, Show a, HasCallStack) => p a -> Text -> a -> Assertion
  ok p i o = run p "filename" i @?= Right o
  err :: (Eq a, Show a, HasCallStack) => p a -> Text -> Assertion
  err p i = assertBool "err" $ isLeft $ run p "filename" i

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

CharParser:
scan
asciiScan

ChunkParser:
elementScan

Element Combinators:
scanElementsWhile
scanElementsWhile1

Char Combinators:
scanCharsWhile
scanCharsWhile1
-}
