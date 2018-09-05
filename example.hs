-- Taken from parsers-bench by @mrkkrp

import System.Environment (getArgs)
import Text.ParParsec
import qualified Data.ByteString as B

data Result a
  = Error String
  | Success a
  deriving (Eq, Show)

data Value
  = Object ![(Text, Value)]
  | Array  ![Value]
  | String !Text
  | Number !Integer !Integer
  | Bool   !Bool
  | Null
  deriving (Eq, Show)

json :: Parser p => p Value
json = space *> (object <|> array)

object :: Parser p => p Value
object = Object <$> (char '{' *> space *> sepBy pair (space *> char ',' *> space) <* space <* char '}')

pair :: Parser p => p (Text, Value)
pair = (,) <$> (text <* space) <*> (char ':' *> space *> value)

array :: Parser p => p Value
array = Array <$> (char '[' *> sepBy value (space *> char ',' *> space) <* space <* char ']')

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
text = char '"' *> asString (skipMany $ satisfy (/= '"')) <* char '"'

space :: Parser p => p ()
space = skipMany (satisfy (\c -> c == ' ' || c == '\n' || c == '\t'))

number :: Parser p => p Value
number = do
  neg <- option id $ negate <$ char '-'
  (c, _, e) <- fractionDec (pure ())
  pure $ Number (neg c) e

main :: IO ()
main = do
  args <- getArgs
  case args of
    [file] -> do
      b <- B.readFile file
      case runParser json file b of
        Left x  -> putStrLn $ showReport x
        Right x -> print x
    _ -> error "Usage: example test.json"