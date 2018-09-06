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
import GHC.Generics (Generic)

-- | Line and column position starting at (1,1)
data Pos = Pos
  { _posLine   :: !Int
  , _posColumn :: !Int
  } deriving (Eq, Show, Generic)

-- | Parsing errors
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
  deriving (Eq, Ord, Show, Generic)

-- | Parser shortcut
type Parser a = (forall p. MonadParser p => p a)

-- | Parser class, which specifies the necessary
-- primitives for parsing. All other parser combinators
-- rely on these primitives.
class (MonadFail p, MonadPlus p) => MonadParser p where
  -- | Get file name associated with current parser
  getFile :: p FilePath

  -- | Get current position of the parser
  getPos :: p Pos

  -- | Get reference position used for indentation-sensitive parsing
  getRefPos :: p Pos

  -- | Update reference position with current position
  withRefPos :: p a -> p a

  -- | Parser which succeeds when the given parser fails
  notFollowedBy :: Show a => p a -> p ()

  -- | Look ahead and return result of the given parser
  -- The current position stays the same.
  lookAhead :: p a -> p a

  -- | Parser failure with detailled 'Error'
  failWith :: Error -> p a

  -- | Parser which succeeds at the end of file
  eof :: p ()

  -- | Annotate the given parser with a label
  -- used for error reporting
  label :: String -> p a -> p a

  -- | Hide errors occurring within the given parser
  -- from the error report. Based on the given
  -- labels an 'Error' is constructed instead.
  hidden :: p a -> p a

  -- | Commit to the given branch, increasing
  -- the priority of the errors within this branch
  -- in contrast to other branches.
  --
  -- This is basically the opposite of the `try`
  -- combinator provided by other parser combinator
  -- libraries, which decreases the error priority
  -- within the given branch (and usually also influences backtracking).
  --
  -- __Note__: `commit` only applies to the reported
  -- errors, it has no effect on the backtracking behavior
  -- of the parser.
  commit :: p a -> p a

  -- | Parse a single byte
  byte :: Word8 -> p Word8

  -- | Parse a single UTF-8 character
  char :: Char -> p Char

  -- | Parse a single character with the given predicate
  satisfy :: (Char -> Bool) -> p Char

  -- | Parse a single byte with the given predicate
  byteSatisfy :: (Word8 -> Bool) -> p Word8

  -- | Parse a string of bytes
  bytes :: ByteString -> p ByteString

  -- | Run the given parser and return the
  -- result as bytes
  asBytes :: p () -> p ByteString

-- | Pretty string representation of 'Error'
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
