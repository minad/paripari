{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE MagicHash #-}
module Text.PariPari.Internal.Acceptor (
  Acceptor(..)
  , Env(..)
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
  , _envFile      :: !FilePath
  , _envEnd       :: {-#UNPACK#-}!Int
  , _envRefLine   :: {-#UNPACK#-}!Int
  , _envRefColumn :: {-#UNPACK#-}!Int
  }

type State = (# Int, Int, Int #)

type Result# a = (# () | (# State, a #) #)
pattern Ok# :: State -> a -> Result# a
pattern Ok# s a = (# | (# s, a #) #)

pattern Err# :: Result# a
pattern Err# = (# () | #)
{-# COMPLETE Ok#, Err# #-}

_stLine :: State -> Int
_stLine (# _, x, _ #) = x
{-# INLINE _stLine #-}

_stColumn :: State -> Int
_stColumn (# _, _, x #) = x
{-# INLINE _stColumn #-}

_stOff :: State -> Int
_stOff (# x, _, _ #) = x
{-# INLINE _stOff #-}

-- | Parser which is optimised for fast parsing. Error reporting
-- is minimal.
newtype Acceptor k a = Acceptor
  { unAcceptor :: Env k -> State -> Result# a }

instance (Chunk k, Semigroup a) => Sem.Semigroup (Acceptor k a) where
  p1 <> p2 = (<>) <$> p1 <*> p2
  {-# INLINE (<>) #-}

instance (Chunk k, Semigroup a, Monoid a) => Monoid (Acceptor k a) where
  mempty = pure mempty
  {-# INLINE mempty #-}

  mappend = (<>)
  {-# INLINE mappend #-}

instance Functor (Acceptor k) where
  fmap f p = Acceptor $ \env st -> case unAcceptor p env st of
    Err# -> Err#
    Ok# st' x -> Ok# st' (f x)
  {-# INLINE fmap #-}

instance Chunk k => Applicative (Acceptor k) where
  pure x = Acceptor $ \_ st -> Ok# st x
  {-# INLINE pure #-}

  f <*> a = Acceptor $ \env st -> case unAcceptor f env st of
    Err# -> Err#
    Ok# s f' -> case unAcceptor a env s of
      Err# -> Err#
      Ok# s' a' -> Ok# s' (f' a')
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
  empty = Acceptor $ \_ _ -> Err#
  {-# INLINE empty #-}

  p1 <|> p2 = Acceptor $ \env st ->
    case unAcceptor p1 env st of
      Ok# st' x -> Ok# st' x
      Err# -> unAcceptor p2 env st
  {-# INLINE (<|>) #-}

instance Chunk k => MonadPlus (Acceptor k)

instance Chunk k => Monad (Acceptor k) where
  p >>= f = Acceptor $ \env st ->
    case unAcceptor p env st of
      Err# -> Err#
      Ok# s x -> unAcceptor (f x) env s
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

  notFollowedBy p = Acceptor $ \env st ->
    case unAcceptor p env st of
      Err# -> Ok# st ()
      Ok# _ _ -> Err#
  {-# INLINE notFollowedBy #-}

  lookAhead p = Acceptor $ \env st -> do
    case unAcceptor p env st of
      Err# -> Err#
      Ok# _ x -> Ok# st x
  {-# INLINE lookAhead #-}

  failWith _ = Acceptor $ \_ _ -> Err#
  {-# INLINE failWith #-}

  eof = Acceptor $ \env st ->
    if _stOff st >= _envEnd env then
      Ok# st ()
    else
      Err#
  {-# INLINE eof #-}

  label _ p = p
  {-# INLINE label #-}

  hidden p = p
  {-# INLINE hidden #-}

  commit p = p
  {-# INLINE commit #-}

  recover p _ = p
  {-# INLINE recover #-}

  element e = Acceptor $ \env (# _stOff, _stLine, _stColumn #) ->
    if | _stOff < _envEnd env,
         (# e', w #) <- elementAt @k (_envBuf env) _stOff,
         e == e',
         pos <- elementPos @k e (Pos _stLine _stColumn) ->
           Ok# (# _stOff + w, _posLine pos, _posColumn pos #) e
       | otherwise ->
           Err#
  {-# INLINE element #-}

  elementScan f = Acceptor $ \env (# _stOff, _stLine, _stColumn #) ->
    if | _stOff < _envEnd env,
         (# e, w #) <- elementAt @k (_envBuf env) _stOff,
         Just r <- f e,
         pos <- elementPos @k e (Pos _stLine _stColumn) ->
           Ok# (# _stOff + w, _posLine pos, _posColumn pos #) r
       | otherwise ->
           Err#
  {-# INLINE elementScan #-}

  chunk k = Acceptor $ \env (# _stOff, _stLine, _stColumn #) ->
    let n = chunkWidth @k k
    in if n + _stOff <= _envEnd env &&
          chunkEqual @k (_envBuf env) _stOff k then
         Ok# (# _stOff + n, _stLine, _stColumn + n #) k
       else
         Err#
  {-# INLINE chunk #-}

  asChunk p = do
    begin <- get (const _stOff)
    p
    end <- get (const _stOff)
    src <- get (\env _ -> _envBuf env)
    pure $ packChunk src begin (end - begin)
  {-# INLINE asChunk #-}

instance Chars k => CharParser k (Acceptor k) where
  scan f = Acceptor $ \env (# _stOff, _stLine, _stColumn #) ->
    if | (# c, w #) <- charAt @k (_envBuf env) _stOff,
         c /= '\0',
         Just r <- f c ->
           Ok# (# _stOff + w,
                 if c == '\n' then _stLine + 1 else _stLine,
                 if c == '\n' then 1 else _stColumn + 1 #) r
       | otherwise ->
           Err#
  {-# INLINE scan #-}

  -- By inling this combinator, GHC should figure out the `charWidth`
  -- of the character resulting in an optimised decoder.
  char '\0' = error "Character '\\0' cannot be parsed because it is used as sentinel"
  char c
    | w <- charWidth @k c =
        Acceptor $ \env (# _stOff, _stLine, _stColumn #) ->
        if charAtFixed @k w (_envBuf env) _stOff == c then
          Ok# (# _stOff + w,
                if c == '\n' then _stLine + 1 else _stLine,
                if c == '\n' then 1 else _stColumn + 1 #) c
        else
          Err#
  {-# INLINE char #-}

  asciiScan f = Acceptor $ \env (# _stOff, _stLine, _stColumn #) ->
    if | b <- byteAt @k (_envBuf env) _stOff,
         b /= 0,
         b < 128,
         Just x <- f b ->
           Ok# (# _stOff + 1
               , if b == asc_newline then _stLine + 1 else _stLine
               , if b == asc_newline then 1 else _stColumn + 1 #) x
       | otherwise ->
           Err#
  {-# INLINE asciiScan #-}

  asciiByte 0 = error "Character '\\0' cannot be parsed because it is used as sentinel"
  asciiByte b
    | b >= 128 = error "Not an ASCII character"
    | otherwise = Acceptor $ \env (# _stOff, _stLine, _stColumn #) ->
        if byteAt @k (_envBuf env) _stOff == b then
          Ok# (# _stOff + 1
              , if b == asc_newline then _stLine + 1 else _stLine
              , if b == asc_newline then 1 else _stColumn + 1 #) b
        else
          Err#
  {-# INLINE asciiByte #-}

instance Chars k => IsString (Acceptor k k) where
  fromString = string
  {-# INLINE fromString #-}

-- | Reader monad, get something from the environment
get :: (Env k -> State -> a) -> Acceptor k a
get f = Acceptor $ \env st -> Ok# st (f env st)
{-# INLINE get #-}

-- | Reader monad, modify environment locally
local :: (State -> Env k -> Env k) -> Acceptor k a -> Acceptor k a
local f p = Acceptor $ \env st ->
  unAcceptor p (f st env) st
{-# INLINE local #-}

-- | Run 'Acceptor' on the given chunk, returning either
-- a simple 'Error' or, if successful, the result.
runAcceptor :: Chunk k => Acceptor k a -> FilePath -> k -> Maybe a
runAcceptor p f k =
  let (# b, off, len #) = unpackChunk k
  in case unAcceptor p (initialEnv f b (off + len)) (# off, 1, 1 #) of
       Err# -> Nothing
       Ok# _ x -> Just x

initialEnv :: FilePath -> Buffer k -> Int -> Env k
initialEnv _envFile _envBuf _envEnd = Env
  { _envBuf
  , _envFile
  , _envEnd
  , _envRefLine = 1
  , _envRefColumn = 1
  }
