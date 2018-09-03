module Text.ParParsec.Class (
  Parser(..)
  , Alternative(..)
  , MonadPlus
  , Pos(..)
  , ByteString
  , Word8
) where

import Control.Applicative (Alternative(empty, (<|>)))
import Control.Monad (MonadPlus(..))
import Control.Monad.Fail (MonadFail(..))
import Data.ByteString (ByteString)
import Data.Word (Word8)

data Pos = Pos
  { _posLine   :: !Int
  , _posColumn :: !Int
  } deriving (Eq, Show)

class (MonadFail p, MonadPlus p) => Parser p where
  getFile :: p FilePath
  getPos :: p Pos
  getRefPos :: p Pos
  setRefPos :: Pos -> p ()
  notFollowedBy :: Show a => p a -> p ()
  lookAhead :: p a -> p a
  eof :: p ()
  label :: String -> p a -> p a
  byte :: Word8 -> p Word8
  char :: Char -> p Char
  satisfy :: (Char -> Bool) -> p Char
  byteSatisfy :: (Word8 -> Bool) -> p Word8
  bytes :: ByteString -> p ByteString
  asBytes :: p () -> p ByteString
