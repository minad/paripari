module Text.ParParsec.Reporter (
  Reporter
  , Report(..)
  , runReporter
  , ErrorContext
  , showErrorContexts
) where

import Control.Monad (void)
import Data.Function (on)
import Data.List (intercalate, nub, sortOn)
import Data.List.NonEmpty (NonEmpty(..))
import Foreign.ForeignPtr (ForeignPtr)
import Text.ParParsec.Ascii
import Text.ParParsec.Class
import Text.ParParsec.Decode
import qualified Control.Monad.Fail as Fail
import qualified Data.ByteString.Internal as B
import qualified Data.List.NonEmpty as NE

type ErrorContext = ([Error], [String])

data Report = Report
  { _reportFile   :: !FilePath
  , _reportLine   :: !Int
  , _reportCol    :: !Int
  , _reportErrors :: [ErrorContext]
  } deriving (Eq, Show)

data Env = Env
  { _envSrc     :: !(ForeignPtr Word8)
  , _envEnd     :: !Int
  , _envFile    :: !FilePath
  , _envHidden  :: !Bool
  , _envCommit  :: !Int
  , _envContext :: [String]
  }

data State = State
  { _stOff       :: !Int
  , _stRefLine   :: !Int
  , _stRefCol    :: !Int
  , _stLine      :: !Int
  , _stCol       :: !Int
  , _stErrOff    :: !Int
  , _stErrLine   :: !Int
  , _stErrCol    :: !Int
  , _stErrCommit :: !Int
  , _stErrors    :: [ErrorContext]
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
  fail msg = failWith $ EFail msg
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

  label l p = local (addLabel l) p
  {-# INLINE label #-}

  hidden p = local (\env -> env { _envHidden = True }) p
  {-# INLINE hidden #-}

  commit p = local (\env -> env { _envCommit = _envCommit env + 1 }) p
  {-# INLINE commit #-}

  notFollowedBy p = Reporter $ \env st ok err ->
    let ok' x _ = raiseError env st err $ EUnexpected $ show x
        err' _ = ok () st
    in unReporter p env st ok' err'
  {-# INLINE notFollowedBy #-}

  lookAhead p = Reporter $ \env st ok err ->
    let ok' x _ = ok x st
    in unReporter p env st ok' err
  {-# INLINE lookAhead #-}

  failWith e = Reporter $ \env st _ err -> raiseError env st err e
  {-# INLINE failWith #-}

  eof = Reporter $ \env st ok err ->
    if _stOff st >= _envEnd env then
      ok () st
    else
      raiseError env st err EExpectedEnd
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
           raiseError env st err $ EExpected [showByte b]
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
              raiseError env st err $ EUnexpected $ showByte b
  {-# INLINE byteSatisfy #-}

  bytes b@(B.PS p i n) = Reporter $ \env st@State{_stOff,_stCol} ok err ->
    if n + _stOff <= _envEnd env &&
       bytesEqual (_envSrc env) _stOff p i n then
      ok b st { _stOff = _stOff + n, _stCol = _stCol + n }
    else
      raiseError env st err $ EExpected [showBytes b]
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
              raiseError env st err $ EUnexpected $ show c
          | c == '\0' && _stOff >= _envEnd env -> err st
          | otherwise -> raiseError env st err EInvalidUtf8
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
        raiseError env st err $ EExpected [show c]
  {-# INLINE char #-}

raiseError :: Env -> State -> (State -> b) -> Error -> b
raiseError env st err e = err $ addError env st e
{-# INLINE raiseError #-}

local :: (Env -> Env) -> Reporter a -> Reporter a
local f p = Reporter $ \env st ok err ->
  unReporter p (f env) st ok err
{-# INLINE local #-}

get :: (Env -> State -> a) -> Reporter a
get f = Reporter $ \env st ok _ -> ok (f env st) st
{-# INLINE get #-}

addLabel :: String -> Env -> Env
addLabel l env = case _envContext env of
  (l':_) | l == l' -> env
  ls               -> env { _envContext = take maxLabelsPerContext $ l : ls }
{-# INLINE addLabel #-}

addError :: Env -> State -> Error -> State
addError env st e
  | _stOff st > _stErrOff st || _envCommit env > _stErrCommit st,
    Just e' <- mkError env e =
      st { _stErrors    = [e']
         , _stErrOff    = _stOff st
         , _stErrLine   = _stLine st
         , _stErrCol    = _stCol st
         , _stErrCommit = _envCommit env
         }
  | _stOff st == _stErrOff st && _envCommit env == _stErrCommit st,
    Just e' <- mkError env e =
      st { _stErrors = shrinkErrors $ e' : _stErrors st }
  | otherwise = st
{-# INLINE addError #-}

mkError :: Env -> Error -> Maybe ErrorContext
mkError env e
  | _envHidden env, (l:ls) <- _envContext env = Just $ ([EExpected [l]], ls)
  | _envHidden env = Nothing
  | otherwise = Just $ ([e], _envContext env)
{-# INLINE mkError #-}

mergeError :: State -> State -> State
mergeError s s'
  | _stErrOff s' > _stErrOff s || _stErrCommit s' > _stErrCommit s =
      s { _stErrors    = _stErrors s'
        , _stErrOff    = _stErrOff s'
        , _stErrLine   = _stErrLine s'
        , _stErrCol    = _stErrCol s'
        , _stErrCommit = _stErrCommit s'
        }
  | _stErrOff s' == _stErrOff s && _stErrCommit s' == _stErrCommit s =
      s { _stErrors = shrinkErrors $ _stErrors s' <> _stErrors s }
  | otherwise = s
{-# INLINE mergeError #-}

groupOn :: Eq e => (a -> e) -> [a] -> [NonEmpty a]
groupOn f = NE.groupBy ((==) `on` f)

shrinkErrors :: [ErrorContext] -> [ErrorContext]
shrinkErrors = take maxErrorContexts . map mergeErrorContexts . groupOn snd . sortOn snd

mergeErrorContexts :: NonEmpty ErrorContext -> ErrorContext
mergeErrorContexts es@((_, ctx):| _) = (take maxErrorsPerContext $ nub $ mergeEExpected $ concatMap fst $ NE.toList es, ctx)

mergeEExpected :: [Error] -> [Error]
mergeEExpected es = [EExpected $ nub expects | not (null expects)] <> filter (null . asEExpected) es
  where expects = concatMap asEExpected es

asEExpected :: Error -> [String]
asEExpected (EExpected s) = s
asEExpected _ = []

maxErrorContexts :: Int
maxErrorContexts = 20

maxErrorsPerContext :: Int
maxErrorsPerContext = 20

maxLabelsPerContext :: Int
maxLabelsPerContext = 5

runReporter :: Reporter a -> FilePath -> ByteString -> Either Report a
runReporter p f t =
  let b = t <> "\0\0\0"
  in unReporter p (initialEnv f b) (initialState b) (\x _ -> Right x) (Left . getReport f)

getReport :: FilePath -> State -> Report
getReport f s = Report f (_stErrLine s) (_stErrCol s) (_stErrors s)

initialEnv :: FilePath -> ByteString -> Env
initialEnv _envFile (B.PS _envSrc off len) = Env
  { _envFile
  , _envSrc
  , _envEnd     = off + len - 3
  , _envContext = []
  , _envHidden  = False
  , _envCommit  = 0
  }

initialState :: ByteString -> State
initialState (B.PS _ _stOff _) = State
  { _stOff
  , _stRefLine   = 0
  , _stRefCol    = 0
  , _stLine      = 1
  , _stCol       = 1
  , _stErrOff    = 0
  , _stErrLine   = 0
  , _stErrCol    = 0
  , _stErrCommit = 0
  , _stErrors    = []
  }

showErrorContexts :: [ErrorContext] -> String
showErrorContexts [] = "No errors"
showErrorContexts es = intercalate "\n" $ map showErrorContext es

showErrorContext :: ErrorContext -> String
showErrorContext (e, c) = intercalate ", " (map showError e) <> showContext c <> "."

showContext :: [String] -> String
showContext [] = ""
showContext xs = " in context of " <> intercalate ", " xs
