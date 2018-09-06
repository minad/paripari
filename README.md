# PariPari: Fast parser combinator library for Haskell

PariPari offers two parsing strategies. There is a fast Acceptor and a slower Reporter which are evaluated in parallel. If the Acceptor fails, the Reporter returns a report about the parsing errors.
This allows for fast parsing in the good case without compromising on the quality of the error messages. I have seen
this idea coming up multiple times before (Trifecta after Attoparsec etc...). However this library provides both parsers
out of the box with equivalent behaviour, in particular with respect to backtracking.

Unlike Parsec and like Attoparsec, the parser combinators backtrack by default. To avoid bad error messages due to the backtracking the `commit :: ChunkParser k p => p a -> p a` parser combinator is provided, which raises the priority of the errors within the given branch. Performance issues can be analyzed by debugging with the tracing parser, which prints messages when backtracking occurs.

PariPari operates on strict `ByteString` and `Text`. If characters are parsed using the `char` and
`satisfy` combinators, bytestrings are interpreted as UTF-8 and decoded on the fly.
As a consequence, PariPari is only a good fit for data which is available at once (no streaming).
In general, the interface of PariPari matches mostly the one of Attoparsec/Megaparsec/etc.

## Features

* Fast-path parser without error reporting (Acceptor)
  and fallback to slower error reporting parser (Reporter)
  to optimize the common case
* Backtracking by default for ease of use
* `commit` combinator to improve error messages by commiting to branches
* Tracing parser to analyze backtracking
* ByteStrings can be parsed as UTF-8
* Combinators for indentation-sensitive parsing
* Most Parsec/Megaparsec combinators provided, relying on parser-combinators

## Specialising all parsers

Performance of PariPari depends crucially on the specialisation of `CharParser k a` to
`Acceptor ByteString a` and `Reporter ByteString a`. In larger parsers it seems that the
GHC specialiser does not kick in. As a workaround we use the `specialise-all` as a
preprocessor script. The script processes our custom `SPECIALISE_ALL` pragmas.

``` haskell
{-# OPTIONS_GHC -F -pgmF ./specialise-all #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}
```

## Example

In this example we use PariPari to parse JSON.

``` haskell
import System.Environment (getArgs)
import Text.PariPari
import qualified Data.ByteString as B
```

We define a datatype of JSON values.

We parametrize the parser with the string type.
Both `ByteString` and `Text` are supported.
Note that even in the case of `ByteStrings` a `CharParser` instance
is provided, which interprets the bytes as UTF-8.

``` haskell
type StringType    = B.ByteString
type ParserMonad p = CharParser StringType p
type Parser a      = (forall p. ParserMonad p => p a)
```

Now we have to ensure that the GHC specialiser kicks in
and specialises all parsers. These pragmas are processed
by the preprocessor script `specialise-all`.

``` haskell
{-# SPECIALISE_ALL ParserMonad p => p ~ Acceptor StringType #-}
{-# SPECIALISE_ALL ParserMonad p => p ~ Reporter StringType #-}
{-# SPECIALISE_ALL Parser => Acceptor StringType #-}
{-# SPECIALISE_ALL Parser => Reporter StringType #-}
```

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

A JSON toplevel value is either an object or an array.

``` haskell
json :: Parser Value
json = space *> (object <|> array) <?> "json"
```

Objects consist of pairs of a text string and a value.

``` haskell
object :: Parser Value
object = Object <$> (char '{' *> space *> sepBy pair (space *> char ',' *> space) <* space <* char '}') <?> "object"

pair :: Parser (StringType, Value)
pair = (,) <$> (text <* space) <*> (char ':' *> space *> value)
```

Arrays are a list of values.

``` haskell
array :: Parser Value
array = Array <$> (char '[' *> sepBy value (space *> char ',' *> space) <* space <* char ']') <?> "array"
```

Furthermore, JSON supports text strings, boolean values, null and floating point numbers.

``` haskell
value :: Parser Value
value =
  (String <$> text)
    <|> object
    <|> array
    <|> (Bool False <$ string "false")
    <|> (Bool True  <$ string "true")
    <|> (Null       <$ string "null")
    <|> number

text :: Parser StringType
text = char '"' *> takeCharsWhile (/= '"') <* char '"' <?> "text"
```

Floating point numbers are parsed by `fractionDec` which returns a coefficient,
the base of the exponent and the exponent. The conversion to `Double` can be done
for example by the `scientific` library.

``` haskell
number :: Parser Value
number = label "number" $ do
  neg <- option id $ negate <$ char '-'
  (c, _, e) <- fractionDec (pure ())
  pure $ Number (neg c) e
```

For spaces we need another helper function.

``` haskell
space :: Parser ()
space = skipCharsWhile (\c -> c == ' ' || c == '\n' || c == '\t')
```

The main function of the example program reads a file, runs the parser
and prints the value if the parsing succeeded.
In the case of an error a report is printed. For demonstration purposes
we also run the tracing parsing with `runTracer`.

``` haskell
main :: IO ()
main = do
  args <- getArgs
  case args of
    [file] -> do
      b <- B.readFile file
      case runCharParser json file b of
        Left x  -> do
          putStrLn $ showReport x
          print $ runTracer json file b
        Right x -> print x
    _ -> error "Usage: example test.json"
```

## Benchmark

See the repository [parsers-bench](https://github.com/minad/parsers-bench/).

```
parsers-bench-0.1.0: benchmarks
Running 1 benchmarks...
Benchmark bench-speed: RUNNING...
benchmarking CSV (Attoparsec)/csv-40.csv
time                 59.43 μs   (56.72 μs .. 63.16 μs)
                     0.986 R²   (0.973 R² .. 0.998 R²)
mean                 57.45 μs   (56.46 μs .. 59.85 μs)
std dev              4.854 μs   (2.479 μs .. 8.184 μs)
variance introduced by outliers: 78% (severely inflated)

benchmarking CSV (Megaparsec)/csv-40.csv
time                 52.52 μs   (51.95 μs .. 53.18 μs)
                     0.999 R²   (0.998 R² .. 0.999 R²)
mean                 53.07 μs   (52.55 μs .. 53.75 μs)
std dev              1.993 μs   (1.391 μs .. 2.720 μs)
variance introduced by outliers: 40% (moderately inflated)

benchmarking CSV (PariPari)/csv-40.csv
time                 16.82 μs   (16.27 μs .. 17.30 μs)
                     0.996 R²   (0.993 R² .. 0.998 R²)
mean                 16.51 μs   (16.25 μs .. 17.23 μs)
std dev              1.334 μs   (669.5 ns .. 2.631 μs)
variance introduced by outliers: 80% (severely inflated)

benchmarking Log (Attoparsec)/log-40.log
time                 373.1 μs   (368.2 μs .. 377.7 μs)
                     0.998 R²   (0.996 R² .. 0.999 R²)
mean                 375.4 μs   (371.1 μs .. 384.0 μs)
std dev              18.81 μs   (8.297 μs .. 30.52 μs)
variance introduced by outliers: 45% (moderately inflated)

benchmarking Log (Megaparsec)/log-40.log
time                 435.7 μs   (418.4 μs .. 453.0 μs)
                     0.993 R²   (0.990 R² .. 0.999 R²)
mean                 420.6 μs   (415.5 μs .. 429.3 μs)
std dev              23.58 μs   (16.95 μs .. 32.19 μs)
variance introduced by outliers: 51% (severely inflated)

benchmarking Log (PariPari)/log-40.log
time                 364.2 μs   (358.8 μs .. 370.6 μs)
                     0.998 R²   (0.996 R² .. 0.999 R²)
mean                 364.8 μs   (359.9 μs .. 375.8 μs)
std dev              24.02 μs   (11.61 μs .. 44.68 μs)
variance introduced by outliers: 60% (severely inflated)

benchmarking JSON (Attoparsec)/json-40.json
time                 22.06 μs   (21.70 μs .. 22.42 μs)
                     0.998 R²   (0.996 R² .. 0.999 R²)
mean                 22.04 μs   (21.81 μs .. 22.61 μs)
std dev              1.122 μs   (592.0 ns .. 2.044 μs)
variance introduced by outliers: 59% (severely inflated)

benchmarking JSON (Megaparsec)/json-40.json
time                 30.40 μs   (29.94 μs .. 30.88 μs)
                     0.998 R²   (0.997 R² .. 0.999 R²)
mean                 30.32 μs   (30.07 μs .. 30.69 μs)
std dev              1.014 μs   (721.0 ns .. 1.466 μs)
variance introduced by outliers: 37% (moderately inflated)

benchmarking JSON (PariPari)/json-40.json
time                 20.44 μs   (20.08 μs .. 20.84 μs)
                     0.997 R²   (0.995 R² .. 0.999 R²)
mean                 20.31 μs   (20.10 μs .. 20.66 μs)
std dev              844.7 ns   (535.8 ns .. 1.171 μs)
variance introduced by outliers: 49% (moderately inflated)

benchmarking JSON (AttoparsecHi)/json-40.json
time                 34.27 μs   (33.85 μs .. 34.75 μs)
                     0.997 R²   (0.995 R² .. 0.998 R²)
mean                 35.27 μs   (34.41 μs .. 36.90 μs)
std dev              3.985 μs   (2.088 μs .. 6.212 μs)
variance introduced by outliers: 87% (severely inflated)

benchmarking JSON (MegaparsecHi)/json-40.json
time                 55.94 μs   (55.21 μs .. 56.65 μs)
                     0.999 R²   (0.997 R² .. 0.999 R²)
mean                 55.61 μs   (55.08 μs .. 56.51 μs)
std dev              2.164 μs   (1.247 μs .. 3.265 μs)
variance introduced by outliers: 42% (moderately inflated)

benchmarking JSON (PariPariHi)/json-40.json
time                 31.46 μs   (31.03 μs .. 32.00 μs)
                     0.996 R²   (0.993 R² .. 0.999 R²)
mean                 32.75 μs   (32.05 μs .. 33.80 μs)
std dev              2.862 μs   (2.049 μs .. 4.450 μs)
variance introduced by outliers: 80% (severely inflated)

Benchmark bench-speed: FINISH
```

## Thanks

* Mark Karpov @mrkkrp - For the parser-combinators library, which was extracted from megaparsec and parsers-bench.
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
