module Text.PariPari (
  module Text.PariPari.Class
  , module Text.PariPari.Combinators
  , module Text.PariPari.Acceptor
  , module Text.PariPari.Reporter
  , runParser
  , runSeqParser
  , runParserWithOptions
  , runSeqParserWithOptions
) where

import Text.PariPari.Acceptor
import Text.PariPari.Class
import Text.PariPari.Combinators
import Text.PariPari.Reporter
import GHC.Conc (par)

-- | Run fast 'Acceptor' and slower 'Reporter' on the given 'ByteString' **in parallel**.
-- The 'FilePath' is used for error reporting.
-- When the acceptor does not return successfully, the result from the reporter
-- is awaited.
runParser :: Parser a -> FilePath -> ByteString -> Either Report a
runParser = runParserWithOptions defaultReportOptions
{-# INLINE runParser #-}
-- Inline to force the specializer to kick in

-- | Run fast 'Acceptor' and slower 'Reporter' on the given 'ByteString' **sequentially**.
-- The 'FilePath' is used for error reporting.
-- When the acceptor does not return successfully, the result from the reporter
-- is awaited.
runSeqParser :: Parser a -> FilePath -> ByteString -> Either Report a
runSeqParser = runSeqParserWithOptions defaultReportOptions
{-# INLINE runSeqParser #-}

-- | Run parsers **in parallel** with additional 'ReportOptions'.
runParserWithOptions :: ReportOptions -> Parser a -> FilePath -> ByteString -> Either Report a
runParserWithOptions o p f b =
  let a = runAcceptor p f b
      r = runReporterWithOptions o p f b
  in case r `par` a of
       Left _  -> r
       Right x -> Right x
{-# INLINE runParserWithOptions #-}

-- | Run parsers **sequentially** with additional 'ReportOptions'.
runSeqParserWithOptions :: ReportOptions -> Parser a -> FilePath -> ByteString -> Either Report a
runSeqParserWithOptions o p f b =
  let a = runAcceptor p f b
      r = runReporterWithOptions o p f b
  in case a of
       Left _  -> r
       Right x -> Right x
{-# INLINE runSeqParserWithOptions #-}
