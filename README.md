# PariPari: Fast parser combinator library for Haskell

PariPari offers two parsing strategies. There is a fast Acceptor and a slower Reporter which are evaluated in parallel. If the Acceptor fails, the Reporter returns a report about the parsing errors.
This allows for fast parsing in the good case without compromising on the quality of the error messages. I have seen
this idea coming up multiple times before (Trifecta after Attoparsec etc...). However this library provides both parsers
out of the box with equivalent behaviour, in particular with respect to backtracking.

Unlike Parsec and like Attoparsec, the parser combinators backtrack by default. To avoid bad error messages due to the backtracking the `commit :: Parser p => p a -> p a` parser combinator is provided, which raises the priority of the errors within the given branch. Performance issues can be analyzed by debugging with the tracing parser, which prints messages when backtracking occurs.

PariPari operates only on strict bytestrings, which are interpreted as UTF-8 if characters are parsed.
Conversion to bytestrings is much cheaper than operating the parser on a suboptimal input format.
In general, the interface of PariPari matches mostly the one of Attoparsec/Megaparsec/etc.

## Note: Issue with specialiser

As of now there is an issue with the GHC specialiser which I have yet to figure out.
Performance of PariPari depends crucially on the specialisation of `forall p. Parser p => p a` to
`Acceptor a` and `Reporter a`. However in larger parsers it seems that the specialiser
does not kick in. As a workaroundI am using the script `gen-parser-specialiser` as a
preprocessor which enforces the specialisation of all parsers.

``` haskell
{-# OPTIONS_GHC -F -pgmF ./ghc-specialise-parser #-}
```

For smaller examples it works as desired though. The benchmarks below were obtained without
the specialiser hack.

## Example

In this example we use PariPari to parse JSON.

``` haskell
import System.Environment (getArgs)
import Text.PariPari
import qualified Data.ByteString as B
```

We define a datatype of JSON values.

``` haskell
data Value
  = Object ![(Text, Value)]
  | Array  ![Value]
  | String !Text
  | Number !Integer !Integer
  | Bool   !Bool
  | Null
  deriving (Eq, Show)
```

A JSON toplevel value is either an object or an array.

``` haskell
json :: Parser p => p Value
json = space *> (object <|> array) <?> "json"
```

Objects consist of pairs of a text string and a value.

``` haskell
object :: Parser p => p Value
object = Object <$> (char '{' *> space *> sepBy pair (space *> char ',' *> space) <* space <* char '}') <?> "object"

pair :: Parser p => p (Text, Value)
pair = (,) <$> (text <* space) <*> (char ':' *> space *> value)
```

Arrays are a list of values.

``` haskell
array :: Parser p => p Value
array = Array <$> (char '[' *> sepBy value (space *> char ',' *> space) <* space <* char ']') <?> "array"
```

Furthermore, JSON supports text strings, boolean values, null and floating point numbers.

``` haskell
value :: Parser p => p Value
value =
  (String <$> text)
    <|> object
    <|> array
    <|> (Bool False <$ string "false")
    <|> (Bool True  <$ string "true")
    <|> (Null       <$ string "null")
    <|> number

text :: Parser p => p Text
text = char '"' *> asString (skipMany $ satisfy (/= '"')) <* char '"' <?> "text"
```

Floating point numbers are parsed by `fractionDec` which returns a coefficient,
the base of the exponent and the exponent. The conversion to `Double` can be done
for example by the `scientific` library.

``` haskell
number :: Parser p => p Value
number = label "number" $ do
  neg <- option id $ negate <$ char '-'
  (c, _, e) <- fractionDec (pure ())
  pure $ Number (neg c) e
```

For spaces we need another helper function.

``` haskell
space :: Parser p => p ()
space = skipMany (satisfy (\c -> c == ' ' || c == '\n' || c == '\t'))
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
      case runParser json file b of
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
time                 57.25 μs   (56.00 μs .. 59.06 μs)
                     0.989 R²   (0.982 R² .. 0.995 R²)
mean                 60.97 μs   (58.73 μs .. 64.68 μs)
std dev              9.113 μs   (6.484 μs .. 13.31 μs)
variance introduced by outliers: 92% (severely inflated)

benchmarking CSV (Megaparsec)/csv-40.csv
time                 56.64 μs   (52.88 μs .. 60.08 μs)
                     0.983 R²   (0.976 R² .. 0.996 R²)
mean                 54.70 μs   (52.98 μs .. 58.54 μs)
std dev              8.040 μs   (3.863 μs .. 15.01 μs)
variance introduced by outliers: 92% (severely inflated)

benchmarking CSV (PariPari)/csv-40.csv
time                 34.33 μs   (33.76 μs .. 35.03 μs)
                     0.998 R²   (0.997 R² .. 0.999 R²)
mean                 34.48 μs   (34.04 μs .. 35.16 μs)
std dev              1.822 μs   (1.206 μs .. 2.906 μs)
variance introduced by outliers: 59% (severely inflated)

benchmarking Log (Attoparsec)/log-40.log
time                 366.5 μs   (359.1 μs .. 374.6 μs)
                     0.997 R²   (0.994 R² .. 0.999 R²)
mean                 367.9 μs   (362.9 μs .. 376.1 μs)
std dev              20.45 μs   (14.12 μs .. 30.09 μs)
variance introduced by outliers: 51% (severely inflated)

benchmarking Log (Megaparsec)/log-40.log
time                 408.5 μs   (401.1 μs .. 416.1 μs)
                     0.996 R²   (0.993 R² .. 0.998 R²)
mean                 412.1 μs   (404.9 μs .. 431.5 μs)
std dev              38.17 μs   (14.03 μs .. 75.77 μs)
variance introduced by outliers: 74% (severely inflated)

benchmarking Log (PariPari)/log-40.log
time                 381.1 μs   (372.4 μs .. 391.9 μs)
                     0.996 R²   (0.995 R² .. 0.999 R²)
mean                 381.4 μs   (376.4 μs .. 389.9 μs)
std dev              20.29 μs   (13.32 μs .. 33.70 μs)
variance introduced by outliers: 48% (moderately inflated)

benchmarking JSON (Attoparsec)/json-40.json
time                 21.49 μs   (21.19 μs .. 21.88 μs)
                     0.996 R²   (0.993 R² .. 0.998 R²)
mean                 22.30 μs   (21.67 μs .. 23.47 μs)
std dev              2.856 μs   (1.854 μs .. 4.347 μs)
variance introduced by outliers: 91% (severely inflated)

benchmarking JSON (Megaparsec)/json-40.json
time                 30.43 μs   (29.87 μs .. 31.05 μs)
                     0.998 R²   (0.997 R² .. 0.999 R²)
mean                 31.03 μs   (30.47 μs .. 32.12 μs)
std dev              2.520 μs   (1.203 μs .. 4.101 μs)
variance introduced by outliers: 78% (severely inflated)

benchmarking JSON (PariPari)/json-40.json
time                 25.99 μs   (25.64 μs .. 26.37 μs)
                     0.997 R²   (0.994 R² .. 0.999 R²)
mean                 27.00 μs   (26.41 μs .. 27.96 μs)
std dev              2.468 μs   (1.798 μs .. 3.597 μs)
variance introduced by outliers: 82% (severely inflated)

benchmarking JSON (AttoparsecHi)/json-40.json
time                 36.31 μs   (34.85 μs .. 38.49 μs)
                     0.980 R²   (0.961 R² .. 0.996 R²)
mean                 35.59 μs   (34.70 μs .. 37.54 μs)
std dev              4.050 μs   (2.561 μs .. 6.658 μs)
variance introduced by outliers: 87% (severely inflated)

benchmarking JSON (MegaparsecHi)/json-40.json
time                 61.36 μs   (57.84 μs .. 68.85 μs)
                     0.727 R²   (0.512 R² .. 0.975 R²)
mean                 76.04 μs   (66.83 μs .. 105.7 μs)
std dev              48.89 μs   (18.95 μs .. 96.34 μs)
variance introduced by outliers: 99% (severely inflated)

benchmarking JSON (PariPariHi)/json-40.json
time                 35.03 μs   (34.58 μs .. 35.53 μs)
                     0.998 R²   (0.997 R² .. 0.999 R²)
mean                 35.40 μs   (34.92 μs .. 36.31 μs)
std dev              2.145 μs   (1.307 μs .. 3.899 μs)
variance introduced by outliers: 65% (severely inflated)

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
