{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
module Text.PariPari.Internal.Tracer (
  Tracer(..)
  , runTracer
) where

import Debug.Trace (trace)
import Text.PariPari.Internal.Class
import Text.PariPari.Internal.Chunk
import Text.PariPari.Internal.Reporter
import qualified Control.Monad.Fail as Fail

-- | Parser which prints trace messages, when backtracking occurs.
newtype Tracer k a = Tracer { unTracer :: Reporter k a }
  deriving (Semigroup, Monoid, Functor, Applicative, MonadPlus, Monad, Fail.MonadFail)

deriving instance CharChunk k => ChunkParser k (Tracer k)
deriving instance CharChunk k => CharParser k (Tracer k)

instance Chunk k => Alternative (Tracer k) where
  empty = Tracer empty

  p1 <|> p2 = Tracer $ Reporter $ \env st ok err ->
    let err' s =
          let width = _stOff s -_stOff st
              next  = unReporter (unTracer p2) env (mergeErrorState env st s) ok err
          in if width > 1 then
               trace ("Back tracking " <> show width <> " bytes at line " <> show (_stLine s)
                       <> ", column " <> show (_stCol s) <> ", context " <> show (_envContext env) <> ": "
                       <> showChunk (packChunk @k (_envBuf env) (_stOff st) width)) next
             else
               next
    in unReporter (unTracer p1) env st ok err'

-- | Run 'Tracer' on the given 'ByteString', returning either
-- an error 'Report' or, if successful, the result.
runTracer :: Chunk k => Tracer k a -> FilePath -> k -> Either Report a
runTracer = runReporter . unTracer
