{-# LANGUAGE Rank2Types #-}
module Text.PariPari.Internal.Run (
  runCharParser
  , runSeqCharParser
  , runCharParserWithOptions
  , runSeqCharParserWithOptions
  , runChunkParser
  , runSeqChunkParser
  , runChunkParserWithOptions
  , runSeqChunkParserWithOptions
) where

import Text.PariPari.Internal.Acceptor
import Text.PariPari.Internal.Class
import Text.PariPari.Internal.Chunk
import Text.PariPari.Internal.Reporter
import GHC.Conc (par)

-- | Run fast 'Acceptor' and slower 'Reporter' on the given chunk **in parallel**.
-- The 'FilePath' is used for error reporting.
-- When the acceptor does not return successfully, the result from the reporter
-- is awaited.
runCharParser :: CharChunk k => (forall p. CharParser k p => p a) -> FilePath -> k -> Either Report a
runCharParser = runCharParserWithOptions defaultReportOptions
{-# INLINE runCharParser #-}

-- | Run fast 'Acceptor' and slower 'Reporter' on the given chunk **sequentially**.
-- The 'FilePath' is used for error reporting.
-- When the acceptor does not return successfully, the result from the reporter
-- is awaited.
runSeqCharParser :: CharChunk k => (forall p. CharParser k p => p a) -> FilePath -> k -> Either Report a
runSeqCharParser = runSeqCharParserWithOptions defaultReportOptions
{-# INLINE runSeqCharParser #-}

-- | Run parsers **in parallel** with additional 'ReportOptions'.
runCharParserWithOptions :: CharChunk k => ReportOptions -> (forall p. CharParser k p => p a) -> FilePath -> k -> Either Report a
runCharParserWithOptions o p f b =
  let a = runAcceptor p f b
      r = runReporterWithOptions o p f b
  in case r `par` a of
       Left _  -> r
       Right x -> Right x
{-# INLINE runCharParserWithOptions #-}

-- | Run parsers **sequentially** with additional 'ReportOptions'.
runSeqCharParserWithOptions :: CharChunk k => ReportOptions -> (forall p. CharParser k p => p a) -> FilePath -> k -> Either Report a
runSeqCharParserWithOptions o p f b =
  let a = runAcceptor p f b
      r = runReporterWithOptions o p f b
  in case a of
       Left _  -> r
       Right x -> Right x
{-# INLINE runSeqCharParserWithOptions #-}

-- | Run fast 'Acceptor' and slower 'Reporter' on the given chunk **in parallel**.
-- The 'FilePath' is used for error reporting.
-- When the acceptor does not return successfully, the result from the reporter
-- is awaited.
runChunkParser :: CharChunk k => (forall p. ChunkParser k p => p a) -> FilePath -> k -> Either Report a
runChunkParser = runCharParserWithOptions defaultReportOptions
{-# INLINE runChunkParser #-}

-- | Run fast 'Acceptor' and slower 'Reporter' on the given chunk **sequentially**.
-- The 'FilePath' is used for error reporting.
-- When the acceptor does not return successfully, the result from the reporter
-- is awaited.
runSeqChunkParser :: Chunk k => (forall p. ChunkParser k p => p a) -> FilePath -> k -> Either Report a
runSeqChunkParser = runSeqChunkParserWithOptions defaultReportOptions
{-# INLINE runSeqChunkParser #-}

-- | Run parsers **in parallel** with additional 'ReportOptions'.
runChunkParserWithOptions :: Chunk k => ReportOptions -> (forall p. ChunkParser k p => p a) -> FilePath -> k -> Either Report a
runChunkParserWithOptions o p f b =
  let a = runAcceptor p f b
      r = runReporterWithOptions o p f b
  in case r `par` a of
       Left _  -> r
       Right x -> Right x
{-# INLINE runChunkParserWithOptions #-}

-- | Run parsers **sequentially** with additional 'ReportOptions'.
runSeqChunkParserWithOptions :: Chunk k => ReportOptions -> (forall p. ChunkParser k p => p a) -> FilePath -> k -> Either Report a
runSeqChunkParserWithOptions o p f b =
  let a = runAcceptor p f b
      r = runReporterWithOptions o p f b
  in case a of
       Left _  -> r
       Right x -> Right x
{-# INLINE runSeqChunkParserWithOptions #-}
