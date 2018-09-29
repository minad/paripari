
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}

import Data.Foldable (for_)
import System.Environment (getArgs)
import Text.PariPari
import qualified Data.ByteString as B

type StringType    = B.ByteString
type ParserMonad p = CharParser StringType p
type Parser a      = (forall p. ParserMonad p => p a)

-- {-# SPECIALISE_ALL ParserMonad p = p ~ Acceptor StringType #-}
-- {-# SPECIALISE_ALL ParserMonad p = p ~ Reporter StringType #-}
-- {-# SPECIALISE_ALL Parser = Acceptor StringType #-}
-- {-# SPECIALISE_ALL Parser = Reporter StringType #-}

data Value
  = Object ![(StringType, Value)]
  | Array  ![Value]
  | String !StringType
  | Number !Integer !Integer
  | Bool   !Bool
  | Null
  deriving (Eq, Show)

json :: Parser Value
json = space *> (object <|> array) <?> "json"

object :: Parser Value
object = Object <$>
  (
    char '{' *>
    recover (space *> sepBy pair (space *> char ',' *> space) <* space <* char '}')
    ([] <$ skipCharsWhile (/= '}') <* char '}')
  ) <?> "object"

pair :: Parser (StringType, Value)
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

text :: Parser StringType
text = char '"' *> takeCharsWhile (/= '"') <* char '"' <?> "text"

number :: Parser Value
number = label "number" $ do
  neg <- sign
  frac <- fractionDec (pure ())
  pure $ case frac of
           Left n -> Number (neg n) 0
           Right (c, _, e) -> Number (neg c) e

space :: Parser ()
space = skipCharsWhile (\c -> c == ' ' || c == '\n' || c == '\t')

main :: IO ()
main = do
  args <- getArgs
  case args of
    [file] -> do
      src <- B.readFile file
      let (result, reports) = runCharParser json file src
      for_ reports $ putStrLn . showReport
      case result of
        Just val -> print val
        Nothing  -> print $ runTracer json file src
    _ -> error "Usage: paripari-example test.json"
