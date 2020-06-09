
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}

import Data.Foldable (for_)
import System.Environment (getArgs)
import Text.PariPari
import qualified Data.ByteString as B

type StringType = B.ByteString
type PMonad p = Parser StringType p
type P a = (forall p. PMonad p => p a)

-- {-# SPECIALISE_ALL PMonad p = p ~ Acceptor StringType #-}
-- {-# SPECIALISE_ALL PMonad p = p ~ Reporter StringType #-}
-- {-# SPECIALISE_ALL P = Acceptor StringType #-}
-- {-# SPECIALISE_ALL P = Reporter StringType #-}

data Value
  = Object ![(StringType, Value)]
  | Array  ![Value]
  | String !StringType
  | Number !Integer !Integer
  | Bool   !Bool
  | Null
  deriving (Eq, Show)

json :: P Value
json = space *> (object <|> array) <?> "json"

object :: P Value
object = Object <$> (char '{' *> space *> sepBy pair (space *> char ',' *> space) <* space <* char '}') <?> "object"

pair :: P (StringType, Value)
pair = (,) <$> (text <* space) <*> (char ':' *> space *> value)

array :: P Value
array = Array <$> (char '[' *> sepBy value (space *> char ',' *> space) <* space <* char ']') <?> "array"

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

number :: P Value
number = label "number" $ do
  neg <- sign
  frac <- fractionDec (pure ())
  pure $ case frac of
           Left n -> Number (neg n) 0
           Right (c, _, e) -> Number (neg c) e

space :: P ()
space = skipCharsWhile (\c -> c == ' ' || c == '\n' || c == '\t')

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
