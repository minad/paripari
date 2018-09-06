
import System.Environment (getArgs)
import Text.PariPari
import qualified Data.ByteString as B

data Value
  = Object ![(Text, Value)]
  | Array  ![Value]
  | String !Text
  | Number !Integer !Integer
  | Bool   !Bool
  | Null
  deriving (Eq, Show)

json :: Parser Value
json = space *> (object <|> array) <?> "json"

object :: Parser Value
object = Object <$> (char '{' *> space *> sepBy pair (space *> char ',' *> space) <* space <* char '}') <?> "object"

pair :: Parser (Text, Value)
pair = (,) <$> (text <* space) <*> (char ':' *> space *> value)

array :: Parser Value
array = Array <$> (char '[' *> sepBy value (space *> char ',' *> space) <* space <* char ']') <?> "array"

value :: Parser Value
value =
  (String <$> text)
    <|> object
    <|> array
    <|> (Bool False <$ string "false")
    <|> (Bool True  <$ string "true")
    <|> (Null       <$ string "null")
    <|> number

text :: Parser Text
text = char '"' *> takeCharsWhile (/= '"') <* char '"' <?> "text"

number :: Parser Value
number = label "number" $ do
  neg <- option id $ negate <$ char '-'
  (c, _, e) <- fractionDec (pure ())
  pure $ Number (neg c) e

space :: Parser ()
space = skipCharsWhile (\c -> c == ' ' || c == '\n' || c == '\t')

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
