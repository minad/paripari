# PariPari: Fast parser combinator library for Haskell

[![Hackage](https://img.shields.io/hackage/v/paripari.svg)](https://hackage.haskell.org/package/paripari)
[![Build Status](https://secure.travis-ci.org/minad/paripari.png?branch=master)](http://travis-ci.org/minad/paripari)

PariPari is a parser combinator library for Haskell. PariPari can be used as
a drop in replacement for the Parsec class of libraries. However be aware that the library
is new and unstable.

PariPari offers two parsing strategies. There is a fast Acceptor and a slower Reporter which are evaluated in parallel. If the Acceptor fails, the Reporter returns a report about the parsing errors.
This allows for fast parsing in the good case without compromising on the quality of the error messages. I have seen
this idea coming up times before, suggesting to use two different libraries (Trifecta after Attoparsec etc...).
This library provides both parsers out of the box with equivalent behaviour, in particular with respect to backtracking.

Like Attoparsec, the parser combinators backtrack by default. To avoid bad error messages the `commit :: ChunkParser k p => p a -> p a` parser combinator is provided, which raises the priority of the errors within the given branch. Performance issues can be analyzed by debugging with the tracing parser, which prints messages when backtracking occurs.

PariPari operates on strict `ByteString` and `Text`.
As a consequence, PariPari is only a good fit for data which is available at once (no streaming).
If characters are parsed using the `char` and `satisfy` combinators, bytestrings are interpreted as UTF-8 and decoded on the fly.
In general, the interface of PariPari matches mostly the one of Attoparsec/Megaparsec/etc.

## Features

* Fast-path parser without error reporting (Acceptor)
  and fallback to slower error reporting parser (Reporter)
  to optimize the common case
* Backtracking by default for ease of use
* `commit` combinator to improve error messages by committing to branches
* Tracing parser to analyze backtracking
* Support for strict UTF-8 `ByteString` and strict `Text`
* Combinators for indentation-sensitive parsing
* Error recovery support via `recover`
* Provides flexible parsers for integers and fractional numbers
  of base 2 to 36 with support for separators between digits
* Most Parsec/Megaparsec combinators provided, relying on the `parser-combinators` library

## Example

In this example we use PariPari to parse JSON. The following is literate haskell.

### Prologue

We specify a preprocessor, language pragmas and the library imports.
Performance of PariPari depends crucially on the specialisation of `CharParser k a` to
`Acceptor ByteString a` and `Reporter ByteString a`. In larger parsers it seems that the
GHC specialiser does not kick in. As a workaround we use `paripari-specialise-all` as a
preprocessor, which generates `SPECIALISE` pragmas from our custom `SPECIALISE_ALL` pragma.
Using the preprocessor is not necessary, however without it I observed 2x-4x slowdowns in parsing speed.

``` haskell
{-# OPTIONS_GHC -F -pgmF paripari-specialise-all #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}

import Data.Foldable (for_)
import System.Environment (getArgs)
import Text.PariPari
import qualified Data.ByteString as B
```

### Basic parser types

We parametrize the parser with the string type.
Both `ByteString` and `Text` are supported.
Note that even in the case of `ByteStrings` a `CharParser` instance
is provided, which interprets the bytes as UTF-8.

``` haskell
type StringType = B.ByteString
type PMonad p = Parser StringType p
type P a = (forall p. PMonad p => p a)
```

The `P` shortcut can be used for simple combinators.
For functions returning parsers, the `PMonad` constraint must
be used for specialization to work, e.g., `char :: PMonad p => Char -> p Char`.

Now we ensure that the GHC specialiser kicks in
and specialises all parsers. These pragmas are processed
by the preprocessor `paripari-specialise-all`.

``` haskell
{-# SPECIALISE_ALL PMonad p = p ~ Acceptor StringType #-}
{-# SPECIALISE_ALL PMonad p = p ~ Reporter StringType #-}
{-# SPECIALISE_ALL P = Acceptor StringType #-}
{-# SPECIALISE_ALL P = Reporter StringType #-}
```

### JSON datatype

We define a datatype of JSON values.

``` haskell
data Value
  = Object ![(StringType, Value)]
  | Array  ![Value]
  | String !StringType
  | Number !Integer !Integer
  | Bool   !Bool
  | Null
  deriving (Eq, Show)
```

### Parsers

A JSON toplevel value is either an object or an array.

``` haskell
json :: P Value
json = space *> (object <|> array) <?> "json"
```

Objects consist of pairs of a text string and a value.

``` haskell
object :: P Value
object = Object <$> (char '{' *> space *> sepBy pair (space *> char ',' *> space) <* space <* char '}') <?> "object"

pair :: P (StringType, Value)
pair = (,) <$> (text <* space) <*> (char ':' *> space *> value)
```

Arrays are a list of values.

``` haskell
array :: P Value
array = Array <$> (char '[' *> sepBy value (space *> char ',' *> space) <* space <* char ']') <?> "array"
```

Furthermore, JSON supports text strings, boolean values, null and floating point numbers.

``` haskell
value :: P Value
value =
  (String <$> text)
    <|> object
    <|> array
    <|> (Bool False <$ string "false")
    <|> (Bool True  <$ string "true")
    <|> (Null       <$ string "null")
    <|> number

text :: P StringType
text = char '"' *> takeCharsWhile (/= '"') <* char '"' <?> "text"
```

Floating point numbers are parsed by `fractionDec` which returns a coefficient,
the base of the exponent and the exponent. The conversion to `Double` can be done
for example by the `scientific` library.

``` haskell
number :: P Value
number = label "number" $ do
  neg <- sign
  frac <- fractionDec (pure ())
  pure $ case frac of
           Left n -> Number (neg n) 0
           Right (c, _, e) -> Number (neg c) e
```

For spaces we need another helper function.

``` haskell
space :: P ()
space = skipCharsWhile (\c -> c == ' ' || c == '\n' || c == '\t')
```

### Main function

The main function of the example program reads a file, runs the parser
and prints the value if the parsing succeeded.
In the case of an error a report is printed.

``` haskell
main :: IO ()
main = do
  args <- getArgs
  case args of
    [file] -> do
      src <- B.readFile file
      let (result, reports) = runParser json file src
      for_ reports $ putStrLn . showReport
      print result
    _ -> error "Usage: paripari-example test.json"
```

## Benchmark

See the repository [parsers-bench](https://github.com/minad/parsers-bench/).

```
parsers-bench-0.1.0: benchmarks
Running 1 benchmarks...
Benchmark bench-speed: RUNNING...
benchmarking CSV (PariPari)/csv-40.csv
time                 16.34 μs   (16.18 μs .. 16.53 μs)
                     0.998 R²   (0.996 R² .. 0.999 R²)
mean                 16.87 μs   (16.41 μs .. 18.64 μs)
std dev              2.703 μs   (569.0 ns .. 5.580 μs)
variance introduced by outliers: 94% (severely inflated)

benchmarking CSV (PariPari, Reporter)/csv-40.csv
time                 104.9 μs   (103.2 μs .. 106.5 μs)
                     0.985 R²   (0.972 R² .. 0.992 R²)
mean                 128.0 μs   (118.4 μs .. 140.0 μs)
std dev              40.73 μs   (30.44 μs .. 51.00 μs)
variance introduced by outliers: 98% (severely inflated)

benchmarking CSV (Attoparsec)/csv-40.csv
time                 57.69 μs   (56.52 μs .. 60.22 μs)
                     0.917 R²   (0.796 R² .. 0.988 R²)
mean                 68.62 μs   (62.95 μs .. 81.74 μs)
std dev              27.44 μs   (15.54 μs .. 49.65 μs)
variance introduced by outliers: 99% (severely inflated)

benchmarking CSV (Megaparsec)/csv-40.csv
time                 52.12 μs   (51.54 μs .. 52.98 μs)
                     0.996 R²   (0.993 R² .. 0.998 R²)
mean                 54.34 μs   (53.25 μs .. 56.03 μs)
std dev              4.705 μs   (3.416 μs .. 6.766 μs)
variance introduced by outliers: 79% (severely inflated)

benchmarking Log (PariPari)/log-40.log
time                 364.4 μs   (359.8 μs .. 369.6 μs)
                     0.998 R²   (0.997 R² .. 0.999 R²)
mean                 362.3 μs   (359.7 μs .. 365.9 μs)
std dev              10.50 μs   (7.760 μs .. 14.60 μs)
variance introduced by outliers: 22% (moderately inflated)

benchmarking Log (PariPari, Reporter)/log-40.log
time                 411.5 μs   (404.1 μs .. 421.8 μs)
                     0.984 R²   (0.964 R² .. 0.996 R²)
mean                 425.2 μs   (411.3 μs .. 453.2 μs)
std dev              60.17 μs   (37.11 μs .. 88.14 μs)
variance introduced by outliers: 87% (severely inflated)

benchmarking Log (Attoparsec)/log-40.log
time                 368.6 μs   (364.2 μs .. 372.5 μs)
                     0.998 R²   (0.997 R² .. 0.999 R²)
mean                 364.6 μs   (360.9 μs .. 371.3 μs)
std dev              16.87 μs   (10.96 μs .. 27.01 μs)
variance introduced by outliers: 42% (moderately inflated)

benchmarking Log (Megaparsec)/log-40.log
time                 412.4 μs   (405.9 μs .. 423.8 μs)
                     0.992 R²   (0.982 R² .. 0.998 R²)
mean                 419.7 μs   (410.9 μs .. 433.7 μs)
std dev              34.86 μs   (21.22 μs .. 51.70 μs)
variance introduced by outliers: 69% (severely inflated)

benchmarking JSON (PariPari)/json-40.json
time                 20.33 μs   (20.08 μs .. 20.68 μs)
                     0.998 R²   (0.997 R² .. 1.000 R²)
mean                 20.48 μs   (20.27 μs .. 21.02 μs)
std dev              1.179 μs   (578.2 ns .. 2.199 μs)
variance introduced by outliers: 65% (severely inflated)

benchmarking JSON (PariPari, Reporter)/json-40.json
time                 76.16 μs   (74.89 μs .. 78.25 μs)
                     0.994 R²   (0.987 R² .. 0.999 R²)
mean                 76.02 μs   (74.68 μs .. 78.57 μs)
std dev              5.598 μs   (3.438 μs .. 8.326 μs)
variance introduced by outliers: 72% (severely inflated)

benchmarking JSON (Attoparsec)/json-40.json
time                 21.67 μs   (21.43 μs .. 21.97 μs)
                     0.999 R²   (0.998 R² .. 0.999 R²)
mean                 21.69 μs   (21.48 μs .. 21.99 μs)
std dev              836.0 ns   (554.7 ns .. 1.371 μs)
variance introduced by outliers: 45% (moderately inflated)

benchmarking JSON (Megaparsec)/json-40.json
time                 31.50 μs   (30.84 μs .. 32.42 μs)
                     0.994 R²   (0.989 R² .. 0.998 R²)
mean                 31.45 μs   (30.88 μs .. 32.42 μs)
std dev              2.377 μs   (1.554 μs .. 3.610 μs)
variance introduced by outliers: 75% (severely inflated)

benchmarking JSON (PariPari, highlevel)/json-40.json
time                 30.63 μs   (30.08 μs .. 31.24 μs)
                     0.998 R²   (0.996 R² .. 0.999 R²)
mean                 30.69 μs   (30.35 μs .. 31.27 μs)
std dev              1.459 μs   (1.056 μs .. 2.053 μs)
variance introduced by outliers: 54% (severely inflated)

benchmarking JSON (PariPari, Reporter, highlevel)/json-40.json
time                 109.8 μs   (107.2 μs .. 113.2 μs)
                     0.995 R²   (0.992 R² .. 0.998 R²)
mean                 110.0 μs   (108.6 μs .. 112.2 μs)
std dev              6.079 μs   (4.499 μs .. 7.572 μs)
variance introduced by outliers: 57% (severely inflated)

benchmarking JSON (Attoparsec, highlevel)/json-40.json
time                 33.94 μs   (33.56 μs .. 34.39 μs)
                     0.998 R²   (0.997 R² .. 0.999 R²)
mean                 34.31 μs   (33.87 μs .. 35.74 μs)
std dev              2.511 μs   (896.2 ns .. 5.032 μs)
variance introduced by outliers: 74% (severely inflated)

benchmarking JSON (Megaparsec, highlevel)/json-40.json
time                 58.52 μs   (56.70 μs .. 60.51 μs)
                     0.993 R²   (0.990 R² .. 0.998 R²)
mean                 57.58 μs   (56.73 μs .. 58.92 μs)
std dev              3.511 μs   (2.575 μs .. 4.710 μs)
variance introduced by outliers: 64% (severely inflated)

Benchmark bench-speed: FINISH
```

## Thanks

* Mark Karpov @mrkkrp - For the `parser-combinators` library,
  which was extracted from Megaparsec and parsers-bench.
  The json example above is adapted from parsers-bench.

## Related projects

* [parser-combinators](http://hackage.haskell.org/package/parser-combinators)
* [parsec](http://hackage.haskell.org/package/parsec)
* [megaparsec](http://hackage.haskell.org/package/megaparsec)
* [attoparsec](http://hackage.haskell.org/package/attoparsec)
* [Earley](http://hackage.haskell.org/package/Earley)
* [trifecta](http://hackage.haskell.org/package/trifecta)
* [parsix](http://hackage.haskell.org/package/parsix)
* [uu-parsinglib](http://hackage.haskell.org/package/uu-parsinglib)

## License

Released under the MIT License.

Copyright (c) 2018 Daniel Mendler
