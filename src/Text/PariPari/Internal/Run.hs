{-# LANGUAGE Rank2Types #-}
module Text.PariPari.Internal.Run (
  runParser
  , runParserWithOptions
) where

import Text.PariPari.Internal.Acceptor
import Text.PariPari.Internal.Class
import Text.PariPari.Internal.Chunk
import Text.PariPari.Internal.Reporter

-- | Rsun fast 'Acceptor' and slower 'Reporter' on the given  sequentially.
-- The 'FilePath' is used for error reporting.
-- When the acceptor does not return successfully, the result from the reporter
-- is awaited.
runParser :: Chunk k => (forall p. Parser k p => p a) -> FilePath -> k -> (Maybe a, [Report])
runParser = runParserWithOptions defaultReportOptions
{-# INLINE runParser #-}

-- | Run parsers **sequentially** with additional 'ReportOptions'.
runParserWithOptions :: Chunk k => ReportOptions -> (forall p. Parser k p => p a) -> FilePath -> k -> (Maybe a, [Report])
runParserWithOptions o p f b =
  let a = runAcceptor p f b
      r = runReporterWithOptions o p f b
  in case a of
       Nothing  -> r
       Just x -> (Just x, [])
{-# INLINE runParserWithOptions #-}
