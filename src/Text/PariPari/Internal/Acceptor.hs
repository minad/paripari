{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UnboxedTuples #-}
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
import GHC.Base hiding (State#)
import GHC.Word
import Text.PariPari.Internal.Chunk
import Text.PariPari.Internal.Class
import qualified Control.Monad.Fail as Fail

data Env k = Env
  { _envBuf     :: !(Buffer k)
  , _envFile    :: !FilePath
  , _envRefLine :: Int#
  , _envRefCol  :: Int#
  }

type State# = (# Int#, Int#, Int# #)

type Result# a = (# Int# | (# State#, a #) #)

pattern Ok# :: State# -> a -> Result# a
pattern Ok# s a = (# | (# s, a #) #)
pattern Err# :: Int# -> Result# a
pattern Err# o = (# o | #)
{-# COMPLETE Ok#, Err# #-}

_stLine :: State# -> Int#
_stLine (# _, x, _ #) = x
{-# INLINE _stLine #-}

_stCol :: State# -> Int#
_stCol (# _, _, x #) = x
{-# INLINE _stCol #-}

_stOff :: State# -> Int#
_stOff (# x, _, _ #) = x
{-# INLINE _stOff #-}

-- | Parser which is optimised for fast parsing. Error reporting
-- is minimal.
newtype Acceptor k a = Acceptor
  { unAcceptor :: Env k -> State# -> Result# a }

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
    Err# o -> Err# o
    Ok# st' x -> Ok# st' (f x)
  {-# INLINE fmap #-}

instance Chunk k => Applicative (Acceptor k) where
  pure x = Acceptor $ \_ st -> Ok# st x
  {-# INLINE pure #-}

  f <*> a = Acceptor $ \env st -> case unAcceptor f env st of
    Err# o -> Err# o
    Ok# s f' -> case unAcceptor a env s of
      Err# o -> Err# o
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
  empty = Acceptor $ \_ st -> Err# (_stOff st)
  {-# INLINE empty #-}

  p1 <|> p2 = Acceptor $ \env st ->
    case unAcceptor p1 env st of
      Ok# st' x -> Ok# st' x
      Err# _ -> unAcceptor p2 env st
  {-# INLINE (<|>) #-}

instance Chunk k => MonadPlus (Acceptor k)

instance Chunk k => Monad (Acceptor k) where
  p >>= f = Acceptor $ \env st ->
    case unAcceptor p env st of
      Err# o -> Err# o
      Ok# s x -> unAcceptor (f x) env s
  {-# INLINE (>>=) #-}

#if !MIN_VERSION_base(4,11,0)
  fail = Fail.fail
  {-# INLINE fail #-}
#endif

instance Chunk k => Fail.MonadFail (Acceptor k) where
  fail msg = failWith $ EFail msg
  {-# INLINE fail #-}

instance Chunk k => Parser k (Acceptor k) where
  getPos = get $ \_ st -> Pos (I# (_stLine st)) (I# (_stCol st))
  {-# INLINE getPos #-}

  getFile = get $ \env _ -> _envFile env
  {-# INLINE getFile #-}

  getRefPos = get $ \env _ -> Pos (I# (_envRefLine env)) (I# (_envRefCol env))
  {-# INLINE getRefPos #-}

  withRefPos p = local (\st env -> env { _envRefLine = _stLine st, _envRefCol = _stCol st }) p
  {-# INLINE withRefPos #-}

  notFollowedBy p = Acceptor $ \env st ->
    case unAcceptor p env st of
      Err# _ -> Ok# st ()
      Ok# _ _ -> Err# (_stOff st)
  {-# INLINE notFollowedBy #-}

  lookAhead p = Acceptor $ \env st -> do
    case unAcceptor p env st of
      Err# _ -> Err# (_stOff st)
      Ok# _ x -> Ok# st x
  {-# INLINE lookAhead #-}

  failWith _ = Acceptor $ \_ st -> Err# (_stOff st)
  {-# INLINE failWith #-}

  eof = Acceptor $ \env st ->
    case indexByte @k (_envBuf env) (_stOff st) `eqWord#` int2Word# 0# of
      1# -> Ok# st ()
      _ -> Err# (_stOff st)
  {-# INLINE eof #-}

  label _ p = p
  {-# INLINE label #-}

  hidden p = p
  {-# INLINE hidden #-}

  recover p _ = p
  {-# INLINE recover #-}

  try p = Acceptor $ \env st ->
    case unAcceptor p env st of
      Ok# st' x -> Ok# st' x
      Err# _ -> Err# (_stOff st)
  {-# INLINE try #-}

  p1 <!> p2 = Acceptor $ \env st ->
    case unAcceptor p1 env st of
      Ok# st' x -> Ok# st' x
      Err# o | 1# <- o ==# _stOff st -> unAcceptor p2 env st
             | otherwise -> Err# o
  {-# INLINE (<!>) #-}

  chunk k = Acceptor $ \env (# stOff, stLine, stCol #) ->
    case matchChunk @k (_envBuf env) stOff k of
      -1# -> Err# stOff
      n -> Ok# (# stOff +# n, stLine, stCol +# n #) k
  {-# INLINE chunk #-}

  asChunk p = do
    I# begin' <- get (const (\s -> I# (_stOff s)))
    p
    I# end' <- get (const (\s -> I# (_stOff s)))
    src <- get (\env _ -> _envBuf env)
    pure $ packChunk src begin' (end' -# begin')
  {-# INLINE asChunk #-}

  scan f = Acceptor $ \env (# stOff, stLine, stCol #) ->
    case indexChar @k (_envBuf env) stOff of
      (# c, w #)
        | 1# <- c `neChar#` '\0'#, Just r <- f (C# c) ->
          Ok# (# stOff +# w,
                case c `eqChar#` '\n'# of 1# -> stLine +# 1#; _ -> stLine,
                case c `eqChar#` '\n'# of 1# -> 1#; _ -> stCol +# 1# #) r
      _ -> Err# stOff
  {-# INLINE scan #-}

  -- By inling this combinator, GHC should figure out the `charWidth`
  -- of the character resulting in an optimised decoder.
  char '\0' = error "Character '\\0' cannot be parsed because it is used as sentinel"
  char c@(C# c') =
    Acceptor $ \env (# stOff, stLine, stCol #) ->
    case matchChar @k (_envBuf env) stOff c' of
      -1# -> Err# stOff
      w -> Ok# (# stOff +# w,
                if c == '\n' then stLine +# 1# else stLine,
                if c == '\n' then 1# else stCol +# 1# #) c
  {-# INLINE char #-}

  asciiScan f = Acceptor $ \env (# stOff, stLine, stCol #) ->
    if | b <- W8# (indexByte @k (_envBuf env) stOff),
         b /= 0,
         b < 128,
         Just x <- f b ->
           Ok# (# stOff +# 1#
               , if b == asc_newline then stLine +# 1# else stLine
               , if b == asc_newline then 1# else stCol +# 1# #) x
       | otherwise ->
           Err# stOff
  {-# INLINE asciiScan #-}

  asciiByte 0 = error "Character '\\0' cannot be parsed because it is used as sentinel"
  asciiByte b
    | b >= 128 = error "Not an ASCII character"
    | otherwise = Acceptor $ \env (# stOff, stLine, stCol #) ->
        if W8# (indexByte @k (_envBuf env) stOff) == b then
          Ok# (# stOff +# 1#
              , if b == asc_newline then stLine +# 1# else stLine
              , if b == asc_newline then 1# else stCol +# 1# #) b
        else
          Err# stOff
  {-# INLINE asciiByte #-}

instance Chunk k => IsString (Acceptor k k) where
  fromString = string
  {-# INLINE fromString #-}

-- | Reader monad, get something from the environment
get :: (Env k -> State# -> a) -> Acceptor k a
get f = Acceptor $ \env st -> Ok# st (f env st)
{-# INLINE get #-}

-- | Reader monad, modify environment locally
local :: (State# -> Env k -> Env k) -> Acceptor k a -> Acceptor k a
local f p = Acceptor $ \env st ->
  unAcceptor p (f st env) st
{-# INLINE local #-}

-- | Run 'Acceptor' on the given chunk, returning either
-- a simple 'Error' or, if successful, the result.
runAcceptor :: Chunk k => Acceptor k a -> FilePath -> k -> Maybe a
runAcceptor p f k =
  let !(# b, off #) = unpackChunk k
  in case unAcceptor p (initialEnv f b) (# off, 1#, 1# #) of
       Err# _ -> Nothing
       Ok# _ x -> Just x

initialEnv :: FilePath -> Buffer k -> Env k
initialEnv _envFile _envBuf = Env
  { _envBuf
  , _envFile
  , _envRefLine = 1#
  , _envRefCol = 1#
  }
