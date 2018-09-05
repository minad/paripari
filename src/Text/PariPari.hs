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

-- Inline to force the specializer to kick in
runParser :: (forall p. Parser p => p a) -> FilePath -> ByteString -> Either Report a
runParser = runParserWithOptions defaultReportOptions
{-# INLINE runParser #-}

runSeqParser :: (forall p. Parser p => p a) -> FilePath -> ByteString -> Either Report a
runSeqParser = runSeqParserWithOptions defaultReportOptions
{-# INLINE runSeqParser #-}

runParserWithOptions :: ReportOptions -> (forall p. Parser p => p a) -> FilePath -> ByteString -> Either Report a
runParserWithOptions o p f b =
  let a = runAcceptor p f b
      r = runReporterWithOptions o p f b
  in case r `par` a of
       Left _  -> r
       Right x -> Right x
{-# INLINE runParserWithOptions #-}

runSeqParserWithOptions :: ReportOptions -> (forall p. Parser p => p a) -> FilePath -> ByteString -> Either Report a
runSeqParserWithOptions o p f b =
  let a = runAcceptor p f b
      r = runReporterWithOptions o p f b
  in case a of
       Left _  -> r
       Right x -> Right x
{-# INLINE runSeqParserWithOptions #-}
