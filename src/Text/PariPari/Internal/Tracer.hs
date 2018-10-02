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

import Data.Semigroup as Sem
import Data.String (IsString)
import Debug.Trace (trace)
import Text.PariPari.Internal.Chunk
import Text.PariPari.Internal.Class
import Text.PariPari.Internal.Reporter
import qualified Control.Monad.Fail as Fail

-- | Parser which prints trace messages, when backtracking occurs.
newtype Tracer k a = Tracer { unTracer :: Reporter k a }
  deriving (Sem.Semigroup, Monoid, Functor, Applicative, MonadPlus, Monad, Fail.MonadFail)

deriving instance CharChunk k => ChunkParser k (Tracer k)
deriving instance CharChunk k => CharParser k (Tracer k)
deriving instance CharChunk k => IsString (Tracer k k)

instance Chunk k => Alternative (Tracer k) where
  empty = Tracer empty

  p1 <|> p2 = Tracer $ Reporter $ \env st ok err ->
    let err' s =
          let width = _stOff s -_stOff st
              next  = unReporter (unTracer p2) env (mergeErrorState env st s) ok err
          in if width > 1 then
               trace ("Backtracking " <> show width <> " bytes at line " <> show (_stLine s)
                       <> ", column " <> show (_stColumn s) <> ", context " <> show (_envContext env) <> ": "
                       <> showChunk (packChunk @k (_envBuf env) (_stOff st) width)) next
             else
               next
    in unReporter (unTracer p1) env st ok err'

-- | Run 'Tracer' on the given chunk, returning the result
-- if successful and reports from error recoveries.
-- In the case of an error, 'Nothing' is returned and the 'Report' list
-- is non-empty.
runTracer :: Chunk k => Tracer k a -> FilePath -> k -> (Maybe a, [Report])
runTracer = runReporter . unTracer
