module Text.PariPari.Class (
  MonadParser(..)
  , Parser
  , Alternative(..)
  , MonadPlus
  , Pos(..)
  , Error(..)
  , ByteString
  , Word8
  , showError
) where

import Control.Applicative (Alternative(empty, (<|>)))
import Control.Monad (MonadPlus(..))
import Control.Monad.Fail (MonadFail(..))
import Data.ByteString (ByteString)
import Data.List (intercalate)
import Data.Word (Word8)

data Pos = Pos
  { _posLine   :: !Int
  , _posColumn :: !Int
  } deriving (Eq, Show)

data Error
  = EEmpty
  | EInvalidUtf8
  | EExpectedEnd
  | EExpected         [String]
  | EUnexpected       String
  | EFail             String
  | ECombinator       String
  | EIndentNotAligned !Int !Int
  | EIndentOverLine   !Int !Int
  | ENotEnoughIndent  !Int !Int
  deriving (Eq, Ord, Show)

type Parser a = (forall p. MonadParser p => p a)

class (MonadFail p, MonadPlus p) => MonadParser p where
  getFile :: p FilePath
  getPos :: p Pos
  getRefPos :: p Pos
  withRefPos :: p a -> p a
  notFollowedBy :: Show a => p a -> p ()
  lookAhead :: p a -> p a
  failWith :: Error -> p a
  eof :: p ()
  label :: String -> p a -> p a
  hidden :: p a -> p a
  commit :: p a -> p a
  byte :: Word8 -> p Word8
  char :: Char -> p Char
  satisfy :: (Char -> Bool) -> p Char
  byteSatisfy :: (Word8 -> Bool) -> p Word8
  bytes :: ByteString -> p ByteString
  asBytes :: p () -> p ByteString
  takeBytes :: Int -> p ByteString

showError :: Error -> String
showError EEmpty                   = "No error"
showError EInvalidUtf8             = "Invalid UTF-8 character found"
showError EExpectedEnd             = "Expected end of file"
showError (EExpected tokens)       = "Expected " <> intercalate ", " tokens
showError (EUnexpected token)      = "Unexpected " <> token
showError (EFail msg)              = msg
showError (ECombinator name)       = "Combinator " <> name <> " failed"
showError (EIndentNotAligned rc c) = "Invalid alignment, expected column " <> show rc <> " expected, got " <> show c
showError (EIndentOverLine   rl l) = "Indentation over line, expected line " <> show rl <> ", got " <> show l
showError (ENotEnoughIndent  rc c) = "Not enough indentation, expected column " <> show rc <> ", got " <> show c
