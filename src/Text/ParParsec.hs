module Text.ParParsec (
  module Text.ParParsec.Class
  , module Text.ParParsec.Combinators
  , module Text.ParParsec.Acceptor
  , module Text.ParParsec.Reporter
  , runParser
  , runParser'
) where

import Text.ParParsec.Acceptor
import Text.ParParsec.Class
import Text.ParParsec.Combinators
import Text.ParParsec.Reporter
import GHC.Conc (par)

-- Inline to force the specializer to kick in
runParser :: (forall p. Parser p => p a) -> FilePath -> ByteString -> Either Report a
runParser p f b =
  let a = runAcceptor p f b
      r = runReporter p f b
  in case r `par` a of
       Left _  -> r
       Right x -> Right x
{-# INLINE runParser #-}

runParser' :: (forall p. Parser p => p a) -> FilePath -> ByteString -> Either Report a
runParser' p f b =
  let a = runAcceptor p f b
      r = runReporter p f b
  in case a of
       Left _  -> r
       Right x -> Right x
{-# INLINE runParser' #-}
