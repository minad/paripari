{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Text.PariPari.Internal.Acceptor (
  Acceptor(..)
  , Env(..)
  , State(..)
  , get
  , local
  , runAcceptor
) where

import Control.Monad (void)
import Data.Semigroup as Sem
import Data.String (IsString(..))
import Text.PariPari.Internal.Chunk
import Text.PariPari.Internal.Class
import qualified Control.Monad.Fail as Fail

data Env k = Env
  { _envBuf       :: !(Buffer k)
  , _envEnd       :: !Int
  , _envFile      :: !FilePath
  , _envRefLine   :: !Int
  , _envRefColumn :: !Int
  }

data State = State
  { _stOff    :: !Int
  , _stLine   :: !Int
  , _stColumn :: !Int
  }

-- | Parser which is optimised for fast parsing. Error reporting
-- is minimal.
newtype Acceptor k a = Acceptor
  { unAcceptor :: forall b. Env k -> State
               -> (a     -> State -> b)
               -> (Error -> b)
               -> b
  }

instance (Chunk k, Semigroup a) => Sem.Semigroup (Acceptor k a) where
  p1 <> p2 = (<>) <$> p1 <*> p2
  {-# INLINE (<>) #-}

instance (Chunk k, Semigroup a, Monoid a) => Monoid (Acceptor k a) where
  mempty = pure mempty
  {-# INLINE mempty #-}

  mappend = (<>)
  {-# INLINE mappend #-}

instance Functor (Acceptor k) where
  fmap f p = Acceptor $ \env st ok err ->
    unAcceptor p env st (ok . f) err
  {-# INLINE fmap #-}

instance Chunk k => Applicative (Acceptor k) where
  pure x = Acceptor $ \_ st ok _ -> ok x st
  {-# INLINE pure #-}

  f <*> a = Acceptor $ \env st ok err ->
    let ok1 f' s =
          let ok2 a' s' = ok (f' a') s'
          in unAcceptor a env s ok2 err
    in unAcceptor f env st ok1 err
  {-# INLINE (<*>) #-}

  p1 *> p2 = do
    void p1
    p2
  {-# INLINE (*>) #-}

  p1 <* p2 = do
    x <- p1
    void p2
    pure x
  {-# INLINE (<*) #-}

instance Chunk k => Alternative (Acceptor k) where
  empty = Acceptor $ \_ _ _ err -> err $ ECombinator "empty"
  {-# INLINE empty #-}

  p1 <|> p2 = Acceptor $ \env st ok err ->
    let err' _ = unAcceptor p2 env st ok err
    in unAcceptor p1 env st ok err'
  {-# INLINE (<|>) #-}

instance Chunk k => MonadPlus (Acceptor k)

instance Chunk k => Monad (Acceptor k) where
  p >>= f = Acceptor $ \env st ok err ->
    let ok' x s = unAcceptor (f x) env s ok err
    in unAcceptor p env st ok' err
  {-# INLINE (>>=) #-}

#if !MIN_VERSION_base(4,11,0)
  fail = Fail.fail
  {-# INLINE fail #-}
#endif

instance Chunk k => Fail.MonadFail (Acceptor k) where
  fail msg = failWith $ EFail msg
  {-# INLINE fail #-}

instance Chunk k => ChunkParser k (Acceptor k) where
  getPos = get $ \_ st -> Pos (_stLine st) (_stColumn st)
  {-# INLINE getPos #-}

  getFile = get $ \env _ -> _envFile env
  {-# INLINE getFile #-}

  getRefPos = get $ \env _ -> Pos (_envRefLine env) (_envRefColumn env)
  {-# INLINE getRefPos #-}

  withRefPos p = local (\st env -> env { _envRefLine = _stLine st, _envRefColumn = _stColumn st }) p
  {-# INLINE withRefPos #-}

  notFollowedBy p = Acceptor $ \env st ok err ->
    let ok' _ _ = err $ ECombinator "notFollowedBy"
        err' _ = ok () st
    in unAcceptor p env st ok' err'
  {-# INLINE notFollowedBy #-}

  lookAhead p = Acceptor $ \env st ok err ->
    let ok' x _ = ok x st
    in unAcceptor p env st ok' err
  {-# INLINE lookAhead #-}

  failWith e = Acceptor $ \_ _ _ err -> err e
  {-# INLINE failWith #-}

  eof = Acceptor $ \env st ok err ->
    if _stOff st >= _envEnd env then
      ok () st
    else
      err expectedEnd
  {-# INLINE eof #-}

  label _ p = p
  {-# INLINE label #-}

  hidden p = p
  {-# INLINE hidden #-}

  commit p = p
  {-# INLINE commit #-}

  recover p _ = p
  {-# INLINE recover #-}

  element e = Acceptor $ \env st@State{_stOff, _stLine, _stColumn} ok err ->
    if | _stOff < _envEnd env,
         (e', w) <- elementAt @k (_envBuf env) _stOff,
         e == e',
         pos <- elementPos @k e (Pos _stLine _stColumn) ->
           ok e st { _stOff = _stOff + w, _stLine = _posLine pos, _stColumn = _posColumn pos }
       | otherwise ->
           err $ ECombinator "element"
  {-# INLINE element #-}

  elementScan f = Acceptor $ \env st@State{_stOff, _stLine, _stColumn} ok err ->
    if | _stOff < _envEnd env,
         (e, w) <- elementAt @k (_envBuf env) _stOff,
         Just r <- f e,
         pos <- elementPos @k e (Pos _stLine _stColumn) ->
           ok r st { _stOff = _stOff + w, _stLine = _posLine pos, _stColumn = _posColumn pos }
       | otherwise ->
           err $ ECombinator "elementScan"
  {-# INLINE elementScan #-}

  chunk k = Acceptor $ \env st@State{_stOff,_stColumn} ok err ->
    let n = chunkWidth @k k
    in if n + _stOff <= _envEnd env &&
          chunkEqual @k (_envBuf env) _stOff k then
         ok k st { _stOff = _stOff + n, _stColumn = _stColumn + n }
       else
         err $ ECombinator "chunk"
  {-# INLINE chunk #-}

  asChunk p = do
    begin <- get (const _stOff)
    p
    end <- get (const _stOff)
    src <- get (\env _ -> _envBuf env)
    pure $ packChunk src begin (end - begin)
  {-# INLINE asChunk #-}

instance CharChunk k => CharParser k (Acceptor k) where
  scan f = Acceptor $ \env st@State{_stOff, _stLine, _stColumn} ok err ->
    if | (c, w) <- charAt @k (_envBuf env) _stOff,
         c /= '\0',
         Just r <- f c ->
           ok r st
           { _stOff = _stOff + w
           , _stLine = if c == '\n' then _stLine + 1 else _stLine
           , _stColumn = if c == '\n' then 1 else _stColumn + 1
           }
       | otherwise ->
           err $ ECombinator "scan"
  {-# INLINE scan #-}

  -- By inling this combinator, GHC should figure out the `charWidth`
  -- of the character resulting in an optimised decoder.
  char '\0' = error "Character '\\0' cannot be parsed because it is used as sentinel"
  char c
    | w <- charWidth @k c =
        Acceptor $ \env st@State{_stOff, _stLine, _stColumn} ok err ->
        if charAtFixed @k w (_envBuf env) _stOff == c then
          ok c st
          { _stOff = _stOff + w
          , _stLine = if c == '\n' then _stLine + 1 else _stLine
          , _stColumn = if c == '\n' then 1 else _stColumn + 1
          }
        else
          err $ ECombinator "char"
  {-# INLINE char #-}

  asciiScan f = Acceptor $ \env st@State{_stOff, _stLine, _stColumn} ok err ->
    if | b <- byteAt @k (_envBuf env) _stOff,
         b /= 0,
         b < 128,
         Just x <- f b ->
           ok x st
           { _stOff = _stOff + 1
           , _stLine = if b == asc_newline then _stLine + 1 else _stLine
           , _stColumn = if b == asc_newline then 1 else _stColumn + 1
           }
       | otherwise ->
           err $ ECombinator "asciiScan"
  {-# INLINE asciiScan #-}

  asciiByte 0 = error "Character '\\0' cannot be parsed because it is used as sentinel"
  asciiByte b
    | b >= 128 = error "Not an ASCII character"
    | otherwise = Acceptor $ \env st@State{_stOff, _stLine, _stColumn} ok err ->
        if byteAt @k (_envBuf env) _stOff == b then
          ok b st
          { _stOff = _stOff + 1
          , _stLine = if b == asc_newline then _stLine + 1 else _stLine
          , _stColumn = if b == asc_newline then 1 else _stColumn + 1
          }
        else
          err $ ECombinator "asciiByte"
  {-# INLINE asciiByte #-}

instance CharChunk k => IsString (Acceptor k k) where
  fromString = string
  {-# INLINE fromString #-}

-- | Reader monad, get something from the environment
get :: (Env k -> State -> a) -> Acceptor k a
get f = Acceptor $ \env st ok _ -> ok (f env st) st
{-# INLINE get #-}

-- | Reader monad, modify environment locally
local :: (State -> Env k -> Env k) -> Acceptor k a -> Acceptor k a
local f p = Acceptor $ \env st ok err ->
  unAcceptor p (f st env) st ok err
{-# INLINE local #-}

-- | Run 'Acceptor' on the given chunk, returning either
-- a simple 'Error' or, if successful, the result.
runAcceptor :: Chunk k => Acceptor k a -> FilePath -> k -> Either Error a
runAcceptor p f k =
  let (b, off, len) = unpackChunk k
  in unAcceptor p (initialEnv f b (off + len)) (initialState off) (\x _ -> Right x) Left

initialEnv :: FilePath -> Buffer k -> Int -> Env k
initialEnv _envFile _envBuf _envEnd = Env
  { _envBuf
  , _envFile
  , _envEnd
  , _envRefLine = 1
  , _envRefColumn = 1
  }

initialState :: Int -> State
initialState _stOff = State
  { _stOff
  , _stLine = 1
  , _stColumn = 1
  }
