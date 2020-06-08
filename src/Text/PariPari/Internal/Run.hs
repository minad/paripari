{-# LANGUAGE Rank2Types #-}
module Text.PariPari.Internal.Run (
  runCharsParser
  , runCharsParserWithOptions
  , runChunkParser
  , runChunkParserWithOptions
) where

import Text.PariPari.Internal.Acceptor
import Text.PariPari.Internal.Class
import Text.PariPari.Internal.Chunk
import Text.PariPari.Internal.Reporter

-- | Run fast 'Acceptor' and slower 'Reporter' on the given chunk sequentially.
-- The 'FilePath' is used for error reporting.
-- When the acceptor does not return successfully, the result from the reporter
-- is awaited.
runCharsParser :: Chars k => (forall p. CharsParser k p => p a) -> FilePath -> k -> (Maybe a, [Report])
runCharsParser = runCharsParserWithOptions defaultReportOptions
{-# INLINE runCharsParser #-}

-- | Run parsers with additional 'ReportOptions'.
runCharsParserWithOptions :: Chars k => ReportOptions -> (forall p. CharsParser k p => p a) -> FilePath -> k -> (Maybe a, [Report])
runCharsParserWithOptions o p f b =
  let a = runAcceptor p f b
      r = runReporterWithOptions o p f b
  in case a of
       Nothing  -> r
       Just x -> (Just x, [])
{-# INLINE runCharsParserWithOptions #-}

-- | Rsun fast 'Acceptor' and slower 'Reporter' on the given chunk sequentially.
-- The 'FilePath' is used for error reporting.
-- When the acceptor does not return successfully, the result from the reporter
-- is awaited.
runChunkParser :: Chunk k => (forall p. ChunkParser k p => p a) -> FilePath -> k -> (Maybe a, [Report])
runChunkParser = runChunkParserWithOptions defaultReportOptions
{-# INLINE runChunkParser #-}

-- | Run parsers **sequentially** with additional 'ReportOptions'.
runChunkParserWithOptions :: Chunk k => ReportOptions -> (forall p. ChunkParser k p => p a) -> FilePath -> k -> (Maybe a, [Report])
runChunkParserWithOptions o p f b =
  let a = runAcceptor p f b
      r = runReporterWithOptions o p f b
  in case a of
       Nothing  -> r
       Just x -> (Just x, [])
{-# INLINE runChunkParserWithOptions #-}
