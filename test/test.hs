{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}
module Main (main) where

import Data.ByteString (ByteString)
import Data.Either (isLeft)
import Data.Text (Text)
import Test.Tasty
import Test.Tasty.HUnit
import Text.PariPari
import Text.PariPari.Internal (textToChunk, asc_a, asc_0)

main :: IO ()
main = defaultMain tests

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

charTests :: forall k p e. (CharParser k p, CharChunk k, Eq e, Show e)
          => (forall a. p a -> FilePath -> k -> Either e a) -> [TestTree]
charTests run =
  [ testGroup "CharParser" $
    [ testCase "satisfy" $ do
        ok (satisfy (== 'a')) "abc" 'a'
        ok (satisfy (== 'a') <* eof) "a" 'a'
        err (satisfy (== 'b')) "abc"

    , testCase "char" $ do
        ok (char 'a') "abc" 'a'
        ok (char 'a' <* eof) "a" 'a'
        err (char 'b') "abc"

    , testCase "asciiSatisfy" $ do
        ok (asciiSatisfy (== asc_a)) "abc" asc_a
        ok (asciiSatisfy (== asc_a) <* eof) "a" asc_a
        err (asciiSatisfy (== asc_0)) "abc"

    , testCase "asciiByte" $ do
        ok (asciiByte asc_a) "abc" asc_a
        ok (asciiByte asc_a <* eof) "a" asc_a
        err (asciiByte asc_0) "abc"
    ]

  , testGroup "Integer Combinators"
    [ testCase "decimal" $ do
        ok @Integer (decimal <* eof) "1234567890" 1234567890
        ok @Integer (decimal <* string "abc" <* eof) "123abc" 123
        err @Integer decimal "abc"

    , testCase "octal" $ do
        ok @Integer (octal <* eof) "12345670" 0o12345670
        ok @Integer (octal <* string "abc" <* eof) "123abc" 0o123
        err @Integer octal "8abc"

    , testCase "hexadecimal" $ do
        ok @Integer (hexadecimal <* eof) "123456789aBcDeF0" 0x123456789ABCDEF0
        ok @Integer (hexadecimal <* string "xyz" <* eof) "123xyz" 0x123
        err @Integer hexadecimal "gabc"
    ]
  ]
  where
  ok :: (Eq a, Show a) => p a -> Text -> a -> Assertion
  ok p i o = run p "filename" (textToChunk @k i) @?= Right o
  err :: (Eq a, Show a) => p a -> Text -> Assertion
  err p i = assertBool "err" $ isLeft $ run p "filename" (textToChunk @k i)

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

    , testCase "failWith" $
        err (failWith EEmpty :: p ()) "abc"

    , testCase "eof" $ do
        ok eof "" ()
        ok (chunk "abc" *> eof) "abc" ()
        err eof "abc"
        err (chunk "ab" *> eof) "abc"

    , testCase "label" $ do
        ok (label "blub" $ element 'a') "abc" 'a'
        err (label "blub" $ element 'b') "abc"

    , testCase "hidden" $ do
        ok (hidden $ element 'a') "abc" 'a'
        err (hidden $ element 'b') "abc"

    , testCase "commit" $ do
        ok (commit $ element 'a') "abc" 'a'
        err (commit $ element 'b') "abc"

    , testCase "elementSatisfy" $ do
        ok (elementSatisfy (== 'a')) "abc" 'a'
        ok (elementSatisfy (== 'a') <* eof) "a" 'a'
        err (elementSatisfy (== 'b')) "abc"

    , testCase "element" $ do
        ok (element 'a') "abc" 'a'
        ok (element 'a' <* eof) "a" 'a'
        err (element 'b') "abc"

    , testCase "chunk" $ do
        ok (chunk "ab") "abc" "ab"
        err (chunk "bc") "abc"

    , testCase "asChunk" $ do
        ok (asChunk (void $ chunk "ab")) "abc" "ab"
        ok (asChunk (void $ anyElement *> anyElement)) "abc" "ab"
        ok (asChunk (skipCount 2 anyElement)) "abc" "ab"
        err (asChunk (void $ chunk "bc")) "abc"
    ]

  , testGroup "MonadFail"
    [ testCase "fail" $ do
        err (failWith EEmpty :: p ()) "abc"
    ]

  , testGroup "Alternative"
    [ testCase "(<|>)" $ do
        ok (element 'b' <|> element 'a' <|> element 'c') "abc" 'a'
        ok (chunk "abd" <|> chunk "abc" <|> chunk "abe") "abcdef" "abc"
    ]
  ]
  where
  ok :: (Eq a, Show a) => p a -> Text -> a -> Assertion
  ok p i o = run p "filename" i @?= Right o
  err :: (Eq a, Show a) => p a -> Text -> Assertion
  err p i = assertBool "err" $ isLeft $ run p "filename" i


{-

Functor
Applicative
Monad
MonadPlus
Alternative:
empty
optional
some

TODO:

endBy1
someTill
sepBy1
sepEndBy1
many
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

(<?>)
getLine
getColumn
withPos
withSpan
getRefColumn
getRefLine
withRefPos
align
indented
line
linefold
notElement
anyElement
digitByte
asciiByte
integer
integer'
digit
signed
fractionHex
fractionDec
char'
notChar
anyChar
alphaNumChar
digitChar
letterChar
lowerChar
upperChar
symbolChar
categoryChar
punctuationChar
spaceChar
asciiChar
skipChars
takeChars
skipCharsWhile
takeCharsWhile
skipCharsWhile1
takeCharsWhile1
takeElements
skipElements
skipElementsWhile
takeElementsWhile
skipElementsWhile1
takeElementsWhile1
string
-}
