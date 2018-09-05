module Text.ParParsec.Acceptor (
  Acceptor
  , runAcceptor
) where

import Control.Monad (void, when)
import Text.ParParsec.Ascii
import Text.ParParsec.Class
import Text.ParParsec.Decode
import Foreign.ForeignPtr (ForeignPtr)
import qualified Control.Monad.Fail as Fail
import qualified Data.ByteString.Internal as B

data Env = Env
  { _envSrc  :: !(ForeignPtr Word8)
  , _envEnd  :: !Int
  , _envFile :: !FilePath
  }

data State = State
  { _stOff     :: !Int
  , _stRefLine :: !Int
  , _stRefCol  :: !Int
  , _stLine    :: !Int
  , _stCol     :: !Int
  }

newtype Acceptor a = Acceptor
  { unAcceptor :: forall b. Env -> State
               -> (a     -> State -> b)
               -> (Error -> b)
               -> b
  }

instance Semigroup a => Semigroup (Acceptor a) where
  p1 <> p2 = (<>) <$> p1 <*> p2
  {-# INLINE (<>) #-}

instance Monoid a => Monoid (Acceptor a) where
  mempty = pure mempty
  {-# INLINE mempty #-}

instance Functor Acceptor where
  fmap f p = Acceptor $ \env st ok err ->
    unAcceptor p env st (ok . f) err
  {-# INLINE fmap #-}

instance Applicative Acceptor where
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

instance Alternative Acceptor where
  empty = Acceptor $ \_ _ _ err -> err EEmpty
  {-# INLINE empty #-}

  p1 <|> p2 = Acceptor $ \env st ok err ->
    let err' _ = unAcceptor p2 env st ok err
    in unAcceptor p1 env st ok err'
  {-# INLINE (<|>) #-}

instance MonadPlus Acceptor

instance Monad Acceptor where
  p >>= f = Acceptor $ \env st ok err ->
    let ok' x s = unAcceptor (f x) env s ok err
    in unAcceptor p env st ok' err
  {-# INLINE (>>=) #-}

  fail msg = Fail.fail msg
  {-# INLINE fail #-}

instance Fail.MonadFail Acceptor where
  fail msg = failWith $ EFail msg
  {-# INLINE fail #-}

instance Parser Acceptor where
  getPos = get $ \_ st -> Pos (_stLine st) (_stCol st)
  {-# INLINE getPos #-}

  getFile = get $ \env _ -> _envFile env
  {-# INLINE getFile #-}

  getRefPos = get $ \_ st -> Pos (_stRefLine st) (_stRefCol st)
  {-# INLINE getRefPos #-}

  setRefPos (Pos l c) = Acceptor $ \_ st ok _ -> ok () st { _stRefLine = l, _stRefCol = c }
  {-# INLINE setRefPos #-}

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
      err EExpectedEnd
  {-# INLINE eof #-}

  label _ p = p
  {-# INLINE label #-}

  hidden p = p
  {-# INLINE hidden #-}

  commit p = p
  {-# INLINE commit #-}

  byte b = Acceptor $ \env st@State{_stOff, _stLine, _stCol} ok err ->
    if | _stOff >= _envEnd env -> err EEmpty
       | b == byteAt (_envSrc env) _stOff ->
           ok b st
           { _stOff =_stOff + 1
           , _stLine = if b == asc_newline then _stLine + 1 else _stLine
           , _stCol = if b == asc_newline then 1 else _stCol + 1
           }
       | otherwise ->
           err $ ECombinator "byte"
  {-# INLINE byte #-}

  byteSatisfy f = Acceptor $ \env st@State{_stOff, _stLine, _stCol} ok err ->
    let b = byteAt (_envSrc env) _stOff
    in if | _stOff >= _envEnd env -> err EEmpty
          | f b ->
              ok b st
              { _stOff =_stOff + 1
              , _stLine = if b == asc_newline then _stLine + 1 else _stLine
              , _stCol = if b == asc_newline then 1 else _stCol + 1
              }
          | otherwise ->
              err $ ECombinator "byteSatisfy"
  {-# INLINE byteSatisfy #-}

  bytes b@(B.PS p i n) = Acceptor $ \env st@State{_stOff,_stCol} ok err ->
    if n + _stOff <= _envEnd env &&
       bytesEqual (_envSrc env) _stOff p i n then
      ok b st { _stOff = _stOff + n, _stCol = _stCol + n }
    else
      err $ ECombinator "bytes"
  {-# INLINE bytes #-}

  asBytes p = do
    begin <- get (const _stOff)
    p
    end <- get (const _stOff)
    src <- get (\env _ -> _envSrc env)
    pure $ B.PS src begin (end - begin)
  {-# INLINE asBytes #-}

  takeBytes n = do
    begin <- get (const _stOff)
    end <- get (\env _ -> _envEnd env)
    when (begin + n > end) $ failWith $ ECombinator "takeBytes"
    src <- get (\env _ -> _envSrc env)
    pure $ B.PS src begin n
  {-# INLINE takeBytes #-}

  satisfy f = Acceptor $ \env st@State{_stOff, _stLine, _stCol} ok err ->
    let (c, w) = utf8Decode (_envSrc env) _stOff
    in if | c /= '\0' ->
            if f c then
              ok c st
              { _stOff =_stOff + w
              , _stLine = if c == '\n' then _stLine + 1 else _stLine
              , _stCol = if c == '\n' then 1 else _stCol + 1
              }
            else
              err $ ECombinator "satisfy"
          | c == '\0' && _stOff >= _envEnd env -> err EEmpty
          | otherwise -> err $ ECombinator "satisfy"
  {-# INLINE satisfy #-}

  char c =
    let w = utf8Width c
    in Acceptor $ \env st@State{_stOff, _stLine, _stCol} ok err ->
      if utf8DecodeFixed w (_envSrc env) _stOff == c then
        ok c st
        { _stOff =_stOff + w
        , _stLine = if c == '\n' then _stLine + 1 else _stLine
        , _stCol = if c == '\n' then 1 else _stCol + 1
        }
      else
        err $ ECombinator "char"
  {-# INLINE char #-}

get :: (Env -> State -> a) -> Acceptor a
get f = Acceptor $ \env st ok _ -> ok (f env st) st
{-# INLINE get #-}

runAcceptor :: Acceptor a -> FilePath -> ByteString -> Either Error a
runAcceptor p f t =
  let b = t <> "\0\0\0"
  in unAcceptor p (initialEnv f b) (initialState b) (\x _ -> Right x) Left

initialEnv :: FilePath -> ByteString -> Env
initialEnv _envFile (B.PS _envSrc off len) = Env
  { _envSrc, _envEnd = off + len - 3, _envFile }

initialState :: ByteString -> State
initialState (B.PS _ _stOff _) = State
  { _stOff
  , _stRefLine = 0
  , _stRefCol = 0
  , _stLine = 1
  , _stCol = 1
  }
