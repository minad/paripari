module Text.PariPari.Reporter (
  Reporter
  , Report(..)
  , runReporter
  , runReporterWithOptions
  , ErrorContext
  , showReport
  , showErrors
  , ReportOptions(..)
  , defaultReportOptions
  , Tracer
  , runTracer
) where

import Control.Monad (void, when)
import Data.Function (on)
import Data.List (intercalate, sort, group, sortOn)
import Data.List.NonEmpty (NonEmpty(..))
import Debug.Trace (trace)
import Foreign.ForeignPtr (ForeignPtr)
import Text.PariPari.Ascii
import Text.PariPari.Class
import Text.PariPari.Decode
import qualified Control.Monad.Fail as Fail
import qualified Data.ByteString.Internal as B
import qualified Data.List.NonEmpty as NE

type ErrorContext = ([Error], [String])

data ReportOptions = ReportOptions
  { _optMaxContexts         :: !Int
  , _optMaxErrorsPerContext :: !Int
  , _optMaxLabelsPerContext :: !Int
  }

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
  , _envOptions :: !ReportOptions
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

instance Semigroup a => Semigroup (Reporter a) where
  p1 <> p2 = (<>) <$> p1 <*> p2
  {-# INLINE (<>) #-}

instance Monoid a => Monoid (Reporter a) where
  mempty = pure mempty
  {-# INLINE mempty #-}

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
    let err' s = unReporter p2 env (mergeError env st s) ok err
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

instance MonadParser Reporter where
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

  takeBytes n = do
    begin <- get (const _stOff)
    end <- get (\env _ -> _envEnd env)
    when (begin + n > end) $ failWith $ EExpected [show n <> " bytes"]
    src <- get (\env _ -> _envSrc env)
    pure $ B.PS src begin n
  {-# INLINE takeBytes #-}

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
  ls               -> env { _envContext = take (_optMaxLabelsPerContext._envOptions $ env) $ l : ls }
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
      st { _stErrors = shrinkErrors env $ e' : _stErrors st }
  | otherwise = st
{-# INLINE addError #-}

mkError :: Env -> Error -> Maybe ErrorContext
mkError env e
  | _envHidden env, (l:ls) <- _envContext env = Just $ ([EExpected [l]], ls)
  | _envHidden env = Nothing
  | otherwise = Just $ ([e], _envContext env)
{-# INLINE mkError #-}

mergeError :: Env -> State -> State -> State
mergeError env s s'
  | _stErrOff s' > _stErrOff s || _stErrCommit s' > _stErrCommit s =
      s { _stErrors    = _stErrors s'
        , _stErrOff    = _stErrOff s'
        , _stErrLine   = _stErrLine s'
        , _stErrCol    = _stErrCol s'
        , _stErrCommit = _stErrCommit s'
        }
  | _stErrOff s' == _stErrOff s && _stErrCommit s' == _stErrCommit s =
      s { _stErrors = shrinkErrors env $ _stErrors s' <> _stErrors s }
  | otherwise = s
{-# INLINE mergeError #-}

groupOn :: Eq e => (a -> e) -> [a] -> [NonEmpty a]
groupOn f = NE.groupBy ((==) `on` f)

shrinkErrors :: Env -> [ErrorContext] -> [ErrorContext]
shrinkErrors env = take (_optMaxContexts._envOptions $ env) . map (mergeErrorContexts env) . groupOn snd . sortOn snd

mergeErrorContexts :: Env -> NonEmpty ErrorContext -> ErrorContext
mergeErrorContexts env es@((_, ctx):| _) = (take (_optMaxErrorsPerContext._envOptions $ env) $ nubSort $ mergeEExpected $ concatMap fst $ NE.toList es, ctx)

mergeEExpected :: [Error] -> [Error]
mergeEExpected es = [EExpected $ nubSort expects | not (null expects)] <> filter (null . asEExpected) es
  where expects = concatMap asEExpected es

nubSort :: Ord a => [a] -> [a]
nubSort = map head . group . sort

asEExpected :: Error -> [String]
asEExpected (EExpected s) = s
asEExpected _ = []

runReporterWithOptions :: ReportOptions -> Reporter a -> FilePath -> ByteString -> Either Report a
runReporterWithOptions o p f t =
  let b = t <> "\0\0\0"
  in unReporter p (initialEnv o f b) (initialState b) (\x _ -> Right x) (Left . getReport f)

runReporter :: Reporter a -> FilePath -> ByteString -> Either Report a
runReporter = runReporterWithOptions defaultReportOptions

getReport :: FilePath -> State -> Report
getReport f s = Report f (_stErrLine s) (_stErrCol s) (_stErrors s)

initialEnv :: ReportOptions -> FilePath -> ByteString -> Env
initialEnv _envOptions _envFile (B.PS _envSrc off len) = Env
  { _envFile
  , _envSrc
  , _envOptions
  , _envEnd     = off + len - 3
  , _envContext = []
  , _envHidden  = False
  , _envCommit  = 0
  }

defaultReportOptions :: ReportOptions
defaultReportOptions = ReportOptions
  { _optMaxContexts         = 20
  , _optMaxErrorsPerContext = 20
  , _optMaxLabelsPerContext = 5
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

showReport :: Report -> String
showReport r =
  "Parser errors at " <> _reportFile r
  <> ", line " <> show (_reportLine r)
  <> ", column " <> show (_reportCol r)
  <> "\n\n" <> showErrors (_reportErrors r)

showErrors :: [ErrorContext] -> String
showErrors [] = "No errors"
showErrors es = intercalate "\n" $ map showErrorContext es

showErrorContext :: ErrorContext -> String
showErrorContext (e, c) = intercalate ", " (map showError e) <> showContext c <> "."

showContext :: [String] -> String
showContext [] = ""
showContext xs = " in context of " <> intercalate ", " xs

newtype Tracer a = Tracer { unTracer :: Reporter a }
  deriving (Semigroup, Monoid, Functor, Applicative, MonadPlus, Monad, Fail.MonadFail, MonadParser)

instance Alternative Tracer where
  empty = Tracer empty

  p1 <|> p2 = Tracer $ Reporter $ \env st ok err ->
    let err' s =
          let width = _stOff s -_stOff st
              next  = unReporter (unTracer p2) env (mergeError env st s) ok err
          in if width > 1 then
               trace ("Back tracking " <> show width <> " bytes at line " <> show (_stLine s)
                       <> ", column " <> show (_stCol s) <> ", context " <> show (_envContext env) <> ": "
                       <> showBytes (B.PS (_envSrc env) (_stOff st) width)) next
             else
               next
    in unReporter (unTracer p1) env st ok err'

runTracer :: Tracer a -> FilePath -> ByteString -> Either Report a
runTracer = runReporter . unTracer
