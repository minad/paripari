{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Text.PariPari.Internal.ElementCombinators (
  -- * Basic combinators
  void
  , (<|>)
  , empty
  , optional

  -- * Control.Monad.Combinators.NonEmpty
  , ON.some
  , ON.endBy1
  , ON.someTill
  , ON.sepBy1
  , ON.sepEndBy1

  -- * Control.Monad.Combinators
  , O.many -- dont use Applicative version for efficiency
  , O.between
  , O.choice
  , O.count
  , O.count'
  , O.eitherP
  , O.endBy
  , O.manyTill
  , O.option
  , O.sepBy
  , O.sepEndBy
  , O.skipMany
  , O.skipSome
  , O.skipCount
  , O.skipManyTill
  , O.skipSomeTill

  -- * PariPari
  , (<?>)
  , getLine
  , getColumn
  , withPos
  , withSpan
  , getRefColumn
  , getRefLine
  , withRefPos
  , align
  , indented
  , line
  , linefold
  , notElement
  , anyElement
  , takeElements
  , skipElements
  , skipElementsWhile
  , takeElementsWhile
  , skipElementsWhile1
  , takeElementsWhile1
) where

import Control.Applicative ((<|>), empty, optional)
import Control.Monad (when)
import Control.Monad.Combinators (skipCount, skipMany)
import Data.Functor (void)
import Prelude hiding (getLine)
import Text.PariPari.Internal.Class
import Text.PariPari.Internal.Chunk
import qualified Control.Monad.Combinators as O
import qualified Control.Monad.Combinators.NonEmpty as ON

type ChunkP k a = (forall p. ChunkParser k p => p a)

-- | Infix alias for 'label'
(<?>) :: ChunkParser k p => p a -> String -> p a
(<?>) = flip label
{-# INLINE (<?>) #-}
infix 0 <?>

-- | Get line number of the reference position
getRefLine :: ChunkP k Int
getRefLine = _posLine <$> getRefPos
{-# INLINE getRefLine #-}

-- | Get column number of the reference position
getRefColumn :: ChunkP k Int
getRefColumn = _posColumn <$> getRefPos
{-# INLINE getRefColumn #-}

-- | Get current line number
getLine :: ChunkP k Int
getLine = _posLine <$> getPos
{-# INLINE getLine #-}

-- | Get current column
getColumn :: ChunkP k Int
getColumn = _posColumn <$> getPos
{-# INLINE getColumn #-}

-- | Decorate the parser result with the current position
withPos :: ChunkParser k p => p a -> p (Pos, a)
withPos p = do
  pos <- getPos
  ret <- p
  pure (pos, ret)
{-# INLINE withPos #-}

type Span = (Pos, Pos)

-- | Decoreate the parser result with the position span
withSpan :: ChunkParser k p => p a -> p (Span, a)
withSpan p = do
  begin <- getPos
  ret <- p
  end <- getPos
  pure ((begin, end), ret)
{-# INLINE withSpan #-}

-- | Parser succeeds on the same line as the reference line
line :: ChunkP k ()
line = do
  l <- getLine
  rl <- getRefLine
  when (l /= rl) $ failWith $ EIndentOverLine rl l
{-# INLINE line #-}

-- | Parser succeeds on the same column as the reference column
align :: ChunkP k ()
align = do
  c <- getColumn
  rc <- getRefColumn
  when (c /= rc) $ failWith $ EIndentNotAligned rc c
{-# INLINE align #-}

-- | Parser succeeds for columns greater than the current reference column
indented :: ChunkP k ()
indented = do
  c <- getColumn
  rc <- getRefColumn
  when (c <= rc) $ failWith $ ENotEnoughIndent rc c
{-# INLINE indented #-}

-- | Parser succeeds either on the reference line or
-- for columns greater than the current reference column
linefold :: ChunkP k ()
linefold = line <|> indented
{-# INLINE linefold #-}

-- | Parser a single byte different from the given one
notElement :: forall k p. ChunkParser k p => Element k -> p (Element k)
notElement e = elementSatisfy @k (/= e) <?> "not " <> showElement @k e
{-# INLINE notElement #-}

-- | Parse an arbitrary byte
anyElement :: ChunkP k (Element k)
anyElement = elementSatisfy (const True)
{-# INLINE anyElement #-}

-- | Skip the next n elements
skipElements :: ChunkParser k p => Int -> p ()
skipElements n = skipCount n anyElement
{-# INLINE skipElements #-}

-- | Take the next n elements and advance the position by n
takeElements :: ChunkParser k p => Int -> p k
takeElements n = asChunk (skipElements n) <?> show n <> " elements"
{-# INLINE takeElements #-}

-- | Skip elements while predicate is true
skipElementsWhile :: ChunkParser k p => (Element k -> Bool) -> p ()
skipElementsWhile f = skipMany (elementSatisfy f)
{-# INLINE skipElementsWhile #-}

-- | Takes elements while predicate is true
takeElementsWhile :: ChunkParser k p => (Element k -> Bool) -> p k
takeElementsWhile f = asChunk (skipElementsWhile f)
{-# INLINE takeElementsWhile #-}

-- | Skip at least one element while predicate is true
skipElementsWhile1 :: ChunkParser k p => (Element k -> Bool) -> p ()
skipElementsWhile1 f = elementSatisfy f *> skipElementsWhile f
{-# INLINE skipElementsWhile1 #-}

-- | Take at least one element while predicate is true
takeElementsWhile1 :: ChunkParser k p => (Element k -> Bool) -> p k
takeElementsWhile1 f = asChunk (skipElementsWhile1 f)
{-# INLINE takeElementsWhile1 #-}
