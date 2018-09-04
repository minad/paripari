module Text.ParParsec.Reporter (
  Reporter
  , Error(..)
  , runReporter
  , showErrors
) where

import Control.Monad (void)
import Data.List (intercalate, nub)
import Foreign.ForeignPtr (ForeignPtr)
import Text.ParParsec.Ascii
import Text.ParParsec.Class
import Text.ParParsec.Decode
import qualified Control.Monad.Fail as Fail
import qualified Data.ByteString.Internal as B

data Error = Error
  { _errLabel :: ![String]
  , _errFile  :: !FilePath
  , _errMsg   :: !String
  , _errLine  :: !Int
  , _errCol   :: !Int
  } deriving (Eq, Show)

data Env = Env
  { _envSrc   :: !(ForeignPtr Word8)
  , _envEnd   :: !Int
  , _envFile  :: !FilePath
  , _envLabel :: [String]
  }

data State = State
  { _stOff     :: !Int
  , _stRefLine :: !Int
  , _stRefCol  :: !Int
  , _stLine    :: !Int
  , _stCol     :: !Int
  , _stErrOff  :: !Int
  , _stError   :: [Error]
  }

newtype Reporter a = Reporter
  { unReporter :: forall b. Env -> State
               -> (a     -> State -> b)
               -> (State -> b)
               -> b
  }

instance Functor Reporter where
  fmap f p = Reporter $ \env st ok err ->
    unReporter p env st (ok . f) err
  {-# INLINE fmap #-}

instance Applicative Reporter where
  pure x = Reporter $ \_ st ok _ -> ok x st
  {-# INLINE pure #-}

  f <*> a = Reporter $ \env st ok err ->
    let ok1 f' s =
          let ok2 a' s' = ok (f' a') s'
          in unReporter a env s ok2 err
    in unReporter f env st ok1 err
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

instance Alternative Reporter where
  empty = Reporter $ \_ st _ err -> err st
  {-# INLINE empty #-}

  p1 <|> p2 = Reporter $ \env st ok err ->
    let err' s' = unReporter p2 env (mergeError st s') ok err
    in unReporter p1 env st ok err'
  {-# INLINE (<|>) #-}

instance MonadPlus Reporter

instance Monad Reporter where
  p >>= f = Reporter $ \env st ok err ->
    let ok' x s = unReporter (f x) env s ok err
    in unReporter p env st ok' err
  {-# INLINE (>>=) #-}

  fail msg = Fail.fail msg
  {-# INLINE fail #-}

instance Fail.MonadFail Reporter where
  fail msg = Reporter $ \env st _ err -> failWith env st err msg
  {-# INLINE fail #-}

instance Parser Reporter where
  getPos = get $ \_ st -> Pos (_stLine st) (_stCol st)
  {-# INLINE getPos #-}

  getFile = get $ \env _ -> _envFile env
  {-# INLINE getFile #-}

  getRefPos = get $ \_ st -> Pos (_stRefLine st) (_stRefCol st)
  {-# INLINE getRefPos #-}

  setRefPos (Pos l c) = Reporter $ \_ st ok _ -> ok () st { _stRefLine = l, _stRefCol = c }
  {-# INLINE setRefPos #-}

  label l p = Reporter $ \env st ok err ->
    unReporter p (addLabel env l) st ok err
  {-# INLINE label #-}

  notFollowedBy p = Reporter $ \env st ok err ->
    let ok' x _ = failWith env st err $ "Unexpected " <> show x
        err' _ = ok () st
    in unReporter p env st ok' err'
  {-# INLINE notFollowedBy #-}

  lookAhead p = Reporter $ \env st ok err ->
    let ok' x _ = ok x st
    in unReporter p env st ok' err
  {-# INLINE lookAhead #-}

  eof = Reporter $ \env st ok err ->
    if _stOff st >= _envEnd env then
      ok () st
    else
      failWith env st err "Expected end of file"
  {-# INLINE eof #-}

  byte b = Reporter $ \env st@State{_stOff, _stLine, _stCol} ok err ->
    if | _stOff >= _envEnd env -> err st
       | b == byteAt (_envSrc env) _stOff ->
           ok b st
           { _stOff =_stOff + 1
           , _stLine = if b == asc_newline then _stLine + 1 else _stLine
           , _stCol = if b == asc_newline then 1 else _stCol + 1
           }
       | otherwise ->
           failWith env st err $ "Expected " <> showByte b
  {-# INLINE byte #-}

  byteSatisfy f = Reporter $ \env st@State{_stOff, _stLine, _stCol} ok err ->
    let b = byteAt (_envSrc env) _stOff
    in if | _stOff >= _envEnd env -> err st
          | f b ->
              ok b st
              { _stOff =_stOff + 1
              , _stLine = if b == asc_newline then _stLine + 1 else _stLine
              , _stCol = if b == asc_newline then 1 else _stCol + 1
              }
          | otherwise ->
              failWith env st err $ "Unexpected " <> showByte b
  {-# INLINE byteSatisfy #-}

  bytes b@(B.PS p i n) = Reporter $ \env st@State{_stOff,_stCol} ok err ->
    if n + _stOff <= _envEnd env &&
       bytesEqual (_envSrc env) _stOff p i n then
      ok b st { _stOff = _stOff + n, _stCol = _stCol + n }
    else
      failWith env st err $ "Expected " <> showBytes b
  {-# INLINE bytes #-}

  asBytes p = do
    begin <- get (const _stOff)
    p
    end <- get (const _stOff)
    src <- get (\env _ -> _envSrc env)
    pure $ B.PS src begin (end - begin)
  {-# INLINE asBytes #-}

  satisfy f = Reporter $ \env st@State{_stOff, _stLine, _stCol} ok err ->
    let (c, w) = utf8Decode (_envSrc env) _stOff
    in if | c /= '\0' ->
            if f c then
              ok c st
              { _stOff =_stOff + w
              , _stLine = if c == '\n' then _stLine + 1 else _stLine
              , _stCol = if c == '\n' then 1 else _stCol + 1
              }
            else
              failWith env st err $ "Unexpected " <> show c
          | c == '\0' && _stOff >= _envEnd env -> err st
          | otherwise -> failWith env st err "Invalid UTF-8 character"
  {-# INLINE satisfy #-}

  char c =
    let w = utf8Width c
    in Reporter $ \env st@State{_stOff, _stLine, _stCol} ok err ->
      if utf8DecodeFixed w (_envSrc env) _stOff == c then
        ok c st
        { _stOff =_stOff + w
        , _stLine = if c == '\n' then _stLine + 1 else _stLine
        , _stCol = if c == '\n' then 1 else _stCol + 1
        }
      else
        failWith env st err $ "Expected " <> show c
  {-# INLINE char #-}

failWith :: Env -> State -> (State -> b) -> String -> b
failWith env st err msg = err $ addError env st msg
{-# INLINE failWith #-}

addLabel :: Env -> String -> Env
addLabel env l = case _envLabel env of
  (l':_) | l == l' -> env
  ls               -> env { _envLabel = l : ls }
{-# INLINE addLabel #-}

addError :: Env -> State -> String -> State
addError env st msg =
  let err = Error (_envLabel env) (_envFile env) msg (_stLine st) (_stCol st)
  in if | _stOff st > _stErrOff st ->
            st { _stError = [err], _stErrOff = _stOff st }
        | _stOff st >= _stErrOff st ->
            st { _stError = take maxErrors $ nub $ err : _stError st }
        | otherwise -> st
{-# INLINE addError #-}

mergeError :: State -> State -> State
mergeError s s'
  | _stErrOff s' > _stErrOff s =
      s { _stError = _stError s', _stErrOff = _stErrOff s' }
  | _stErrOff s' == _stErrOff s =
      s { _stError = take maxErrors $ nub $ _stError s' <> _stError s }
  | otherwise = s
{-# INLINE mergeError #-}

maxErrors :: Int
maxErrors = 20

get :: (Env -> State -> a) -> Reporter a
get f = Reporter $ \env st ok _ -> ok (f env st) st
{-# INLINE get #-}

runReporter :: Reporter a -> FilePath -> ByteString -> Either [Error] a
runReporter p f t =
  let b = t <> "\0\0\0"
  in unReporter p (initialEnv f b) (initialState b) (\x _ -> Right x) (Left . _stError)

initialEnv :: FilePath -> ByteString -> Env
initialEnv _envFile (B.PS _envSrc off len) = Env
  { _envFile, _envSrc, _envEnd = off + len - 3, _envLabel = [] }

initialState :: ByteString -> State
initialState (B.PS _ _stOff _) = State
  { _stOff
  , _stRefLine = 0
  , _stRefCol = 0
  , _stLine = 1
  , _stCol = 1
  , _stErrOff = 0
  , _stError = []
  }

showErrors :: [Error] -> String
showErrors e = errorsS e ""

errorsS :: [Error] -> ShowS
errorsS [] = ("No errors" <>)
errorsS xs = go xs
  where go [] = id
        go (e:es) = (_errMsg e <>) . ('.':) . labelS (_errLabel e) . ('\n':) . go es

labelS :: [String] -> ShowS
labelS [] = id
labelS (x:xs) = (" Expected " <>) . (x <>) . context xs . ('.':)
  where context [] = id
        context ys = (" in context of " <>) . (intercalate ", " ys <>)
