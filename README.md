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

## Note: Issue with specialiser

As of now there is an issue with the GHC specialiser which I have yet to figure out.
Performance of PariPari depends crucially on the specialisation of `CharParser k a` to
`Acceptor ByteString a` and `Reporter ByteString a`. In larger parsers it seems that the specialiser
does not kick in. As a workaround I am using the script `gen-parser-specialiser` as a
preprocessor which enforces the specialisation of all parsers.

```
{-# OPTIONS_GHC -F -pgmF ./ghc-specialise-parser #-}
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
type StringType = B.ByteString
type Parser a = (forall p. CharParser StringType p => p a)
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
time                 55.11 μs   (54.41 μs .. 55.82 μs)
                     0.995 R²   (0.990 R² .. 0.998 R²)
mean                 56.28 μs   (55.23 μs .. 57.94 μs)
std dev              4.396 μs   (2.761 μs .. 6.699 μs)
variance introduced by outliers: 75% (severely inflated)

benchmarking CSV (Megaparsec)/csv-40.csv
time                 51.34 μs   (50.67 μs .. 52.25 μs)
                     0.998 R²   (0.997 R² .. 0.999 R²)
mean                 51.36 μs   (50.87 μs .. 52.19 μs)
std dev              2.088 μs   (1.458 μs .. 3.057 μs)
variance introduced by outliers: 44% (moderately inflated)

benchmarking CSV (PariPari)/csv-40.csv
time                 32.51 μs   (32.04 μs .. 33.24 μs)
                     0.993 R²   (0.986 R² .. 0.997 R²)
mean                 35.48 μs   (33.98 μs .. 39.06 μs)
std dev              7.580 μs   (5.010 μs .. 11.12 μs)
variance introduced by outliers: 96% (severely inflated)

benchmarking Log (Attoparsec)/log-40.log
time                 356.2 μs   (350.8 μs .. 363.4 μs)
                     0.997 R²   (0.994 R² .. 0.999 R²)
mean                 359.9 μs   (355.9 μs .. 369.9 μs)
std dev              18.90 μs   (11.41 μs .. 31.91 μs)
variance introduced by outliers: 48% (moderately inflated)

benchmarking Log (Megaparsec)/log-40.log
time                 405.1 μs   (397.8 μs .. 416.0 μs)
                     0.981 R²   (0.953 R² .. 0.996 R²)
mean                 425.9 μs   (410.6 μs .. 457.1 μs)
std dev              69.50 μs   (39.64 μs .. 122.9 μs)
variance introduced by outliers: 90% (severely inflated)

benchmarking Log (PariPari)/log-40.log
time                 354.4 μs   (348.6 μs .. 359.9 μs)
                     0.998 R²   (0.997 R² .. 0.999 R²)
mean                 356.3 μs   (351.7 μs .. 363.8 μs)
std dev              19.99 μs   (13.45 μs .. 29.56 μs)
variance introduced by outliers: 51% (severely inflated)

benchmarking JSON (Attoparsec)/json-40.json
time                 21.70 μs   (21.21 μs .. 22.34 μs)
                     0.989 R²   (0.973 R² .. 0.998 R²)
mean                 21.89 μs   (21.39 μs .. 23.13 μs)
std dev              2.436 μs   (1.496 μs .. 4.176 μs)
variance introduced by outliers: 88% (severely inflated)

benchmarking JSON (Megaparsec)/json-40.json
time                 29.97 μs   (29.60 μs .. 30.33 μs)
                     0.999 R²   (0.998 R² .. 0.999 R²)
mean                 29.72 μs   (29.47 μs .. 30.07 μs)
std dev              950.7 ns   (722.5 ns .. 1.238 μs)
variance introduced by outliers: 35% (moderately inflated)

benchmarking JSON (PariPari)/json-40.json
time                 21.32 μs   (20.84 μs .. 21.93 μs)
                     0.995 R²   (0.990 R² .. 0.998 R²)
mean                 21.53 μs   (21.12 μs .. 22.12 μs)
std dev              1.610 μs   (1.166 μs .. 2.254 μs)
variance introduced by outliers: 76% (severely inflated)

benchmarking JSON (AttoparsecHi)/json-40.json
time                 33.40 μs   (33.07 μs .. 33.82 μs)
                     0.998 R²   (0.996 R² .. 1.000 R²)
mean                 33.54 μs   (33.20 μs .. 34.22 μs)
std dev              1.572 μs   (1.063 μs .. 2.520 μs)
variance introduced by outliers: 53% (severely inflated)

benchmarking JSON (MegaparsecHi)/json-40.json
time                 57.42 μs   (55.73 μs .. 59.79 μs)
                     0.993 R²   (0.988 R² .. 0.998 R²)
mean                 56.81 μs   (55.90 μs .. 58.18 μs)
std dev              3.609 μs   (2.488 μs .. 4.880 μs)
variance introduced by outliers: 66% (severely inflated)

benchmarking JSON (PariPariHi)/json-40.json
time                 31.71 μs   (31.10 μs .. 32.38 μs)
                     0.998 R²   (0.996 R² .. 0.999 R²)
mean                 31.57 μs   (31.23 μs .. 32.33 μs)
std dev              1.650 μs   (903.4 ns .. 2.947 μs)
variance introduced by outliers: 59% (severely inflated)

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
