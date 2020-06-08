{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Text.PariPari.Internal.Reporter (
  Reporter(..)
  , Env(..)
  , State(..)
  , local
  , get
  , raiseError
  , mergeErrorState
  , Report(..)
  , runReporter
  , runReporterWithOptions
  , ErrorContext(..)
  , showReport
  , showErrors
  , ReportOptions(..)
  , defaultReportOptions
) where

import Control.Monad (void)
import Data.Function (on)
import Data.List (intercalate, sort, group, sortOn)
import Data.List.NonEmpty (NonEmpty(..))
import Data.Semigroup as Sem
import Data.String (IsString(..))
import GHC.Generics (Generic)
import Text.PariPari.Internal.Chunk
import Text.PariPari.Internal.Class
import qualified Control.Monad.Fail as Fail
import qualified Data.List.NonEmpty as NE

data ErrorContext = ErrorContext
  { _ecErrors  :: [Error]
  , _ecContext :: [String]
  } deriving (Eq, Show, Generic)

data ReportOptions = ReportOptions
  { _optMaxContexts         :: {-#UNPACK#-}!Int
  , _optMaxErrorsPerContext :: {-#UNPACK#-}!Int
  , _optMaxLabelsPerContext :: {-#UNPACK#-}!Int
  } deriving (Eq, Show, Generic)

data Report = Report
  { _reportFile   :: !FilePath
  , _reportErrors :: [ErrorContext]
  , _reportLine   :: {-#UNPACK#-}!Int
  , _reportColumn :: {-#UNPACK#-}!Int
  } deriving (Eq, Show, Generic)

data Env k = Env
  { _envBuf       :: !(Buffer k)
  , _envFile      :: !FilePath
  , _envOptions   :: !ReportOptions
  , _envHidden    :: !Bool
  , _envContext   :: [String]
  , _envCommit    :: {-#UNPACK#-}!Int
  , _envEnd       :: {-#UNPACK#-}!Int
  , _envRefLine   :: {-#UNPACK#-}!Int
  , _envRefColumn :: {-#UNPACK#-}!Int
  }

data State = State
  { _stOff       :: {-#UNPACK#-}!Int
  , _stLine      :: {-#UNPACK#-}!Int
  , _stColumn    :: {-#UNPACK#-}!Int
  , _stErrOff    :: {-#UNPACK#-}!Int
  , _stErrLine   :: {-#UNPACK#-}!Int
  , _stErrColumn :: {-#UNPACK#-}!Int
  , _stErrCommit :: {-#UNPACK#-}!Int
  , _stErrors    :: [ErrorContext]
  , _stReports   :: [Report]
  }

-- | Parser which is optimised for good error reports.
-- Performance is secondary, since the 'Reporter' is used
-- as a fallback to the 'Acceptor'.
newtype Reporter k a = Reporter
  { unReporter :: forall b. Env k -> State
               -> (a     -> State -> b)
               -> (State -> b)
               -> b
  }

instance (Chunk k, Semigroup a) => Sem.Semigroup (Reporter k a) where
  p1 <> p2 = (<>) <$> p1 <*> p2
  {-# INLINE (<>) #-}

instance (Chunk k, Semigroup a, Monoid a) => Monoid (Reporter k a) where
  mempty = pure mempty
  {-# INLINE mempty #-}

  mappend = (<>)
  {-# INLINE mappend #-}

instance Chunk k => Functor (Reporter k) where
  fmap f p = Reporter $ \env st ok err ->
    unReporter p env st (ok . f) err
  {-# INLINE fmap #-}

instance Chunk k => Applicative (Reporter k) where
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

instance Chunk k => Alternative (Reporter k) where
  empty = Reporter $ \_ st _ err -> err st
  {-# INLINE empty #-}

  p1 <|> p2 = Reporter $ \env st ok err ->
    let err' s = unReporter p2 env (mergeErrorState env st s) ok err
    in unReporter p1 env st ok err'
  {-# INLINE (<|>) #-}

instance Chunk k => MonadPlus (Reporter k)

instance Chunk k => Monad (Reporter k) where
  p >>= f = Reporter $ \env st ok err ->
    let ok' x s = unReporter (f x) env s ok err
    in unReporter p env st ok' err
  {-# INLINE (>>=) #-}

#if !MIN_VERSION_base(4,11,0)
  fail = Fail.fail
  {-# INLINE fail #-}
#endif

instance Chunk k => Fail.MonadFail (Reporter k) where
  fail msg = failWith $ EFail msg
  {-# INLINE fail #-}

instance Chunk k => ChunkParser k (Reporter k) where
  getPos = get $ \_ st -> Pos (_stLine st) (_stColumn st)
  {-# INLINE getPos #-}

  getFile = get $ \env _ -> _envFile env
  {-# INLINE getFile #-}

  getRefPos = get $ \env _ -> Pos (_envRefLine env) (_envRefColumn env)
  {-# INLINE getRefPos #-}

  withRefPos p = local (\st env -> env { _envRefLine = _stLine st, _envRefColumn = _stColumn st }) p
  {-# INLINE withRefPos #-}

  label l p = local (const $ addLabel l) p
  {-# INLINE label #-}

  hidden p = local (const $ \env -> env { _envHidden = True }) p
  {-# INLINE hidden #-}

  commit p = local (const $ \env -> env { _envCommit = _envCommit env + 1 }) p
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
      raiseError env st err expectedEnd
  {-# INLINE eof #-}

  recover p r = Reporter $ \env st ok err ->
    let err1 s =
          let err2 _ = err s
          in unReporter r env (addReport env s) ok err2
    in unReporter p env st ok err1
  {-# INLINE recover #-}

  element e = Reporter $ \env st@State{_stOff, _stLine, _stColumn} ok err ->
    if | _stOff < _envEnd env,
         (e', w) <- elementAt @k (_envBuf env) _stOff,
         e == e',
         pos <- elementPos @k e (Pos _stLine _stColumn) ->
           ok e st { _stOff =_stOff + w, _stLine = _posLine pos, _stColumn = _posColumn pos }
       | otherwise ->
           raiseError env st err $ EExpected [showElement @k e]
  {-# INLINE element #-}

  elementScan f = Reporter $ \env st@State{_stOff, _stLine, _stColumn} ok err ->
    let (e, w) = elementAt @k (_envBuf env) _stOff
    in if | _stOff >= _envEnd env ->
              raiseError env st err unexpectedEnd
          | _stOff < _envEnd env,
            Just r <- f e,
            pos <- elementPos @k e (Pos _stLine _stColumn) ->
              ok r st { _stOff =_stOff + w, _stLine = _posLine pos, _stColumn = _posColumn pos }
          | otherwise ->
              raiseError env st err $ EUnexpected $ showElement @k e
  {-# INLINE elementScan #-}

  chunk k = Reporter $ \env st@State{_stOff,_stColumn} ok err ->
    let n = chunkWidth @k k
    in if n + _stOff <= _envEnd env &&
          chunkEqual @k (_envBuf env) _stOff k then
         ok k st { _stOff = _stOff + n, _stColumn = _stColumn + n }
       else
         raiseError env st err $ EExpected [showChunk @k k]
  {-# INLINE chunk #-}

  asChunk p = do
    begin <- get (const _stOff)
    p
    end <- get (const _stOff)
    src <- get (\env _ -> _envBuf env)
    pure $ packChunk src begin (end - begin)
  {-# INLINE asChunk #-}

instance Chars k => CharsParser k (Reporter k) where
  scan f = Reporter $ \env st@State{_stOff, _stLine, _stColumn} ok err ->
    if | (c, w) <- charAt @k (_envBuf env) _stOff,
         c /= '\0' ->
           case f c of
             Just r ->
               ok r st
               { _stOff = _stOff + w
               , _stLine = if c == '\n' then _stLine + 1 else _stLine
               , _stColumn = if c == '\n' then 1 else _stColumn + 1
               }
             Nothing ->
               raiseError env st err $ EUnexpected $ show c
       | _stOff >= _envEnd env ->
           raiseError env st err unexpectedEnd
       | otherwise ->
           raiseError env st err EInvalidUtf8
  {-# INLINE scan #-}

  -- By inling this combinator, GHC should figure out the `charWidth`
  -- of the character resulting in an optimised decoder.
  char '\0' = error "Character '\\0' cannot be parsed because it is used as sentinel"
  char c
    | w <- charWidth @k c =
        Reporter $ \env st@State{_stOff, _stLine, _stColumn} ok err ->
        if charAtFixed @k w (_envBuf env) _stOff == c then
          ok c st
          { _stOff = _stOff + w
          , _stLine = if c == '\n' then _stLine + 1 else _stLine
          , _stColumn = if c == '\n' then 1 else _stColumn + 1
          }
        else
          raiseError env st err $ EExpected [show c]
  {-# INLINE char #-}

  asciiScan f = Reporter $ \env st@State{_stOff, _stLine, _stColumn} ok err ->
    let b = byteAt @k (_envBuf env) _stOff
    in if | b /= 0,
            b < 128,
            Just x <- f b ->
              ok x st
              { _stOff = _stOff + 1
              , _stLine = if b == asc_newline then _stLine + 1 else _stLine
              , _stColumn = if b == asc_newline then 1 else _stColumn + 1
              }
          | _stOff >= _envEnd env ->
              raiseError env st err unexpectedEnd
          | otherwise ->
              raiseError env st err $ EUnexpected $ showByte b
  {-# INLINE asciiScan #-}

  asciiByte 0 = error "Character '\\0' cannot be parsed because it is used as sentinel"
  asciiByte b
    | b >= 128 = error "Not an ASCII character"
    | otherwise = Reporter $ \env st@State{_stOff, _stLine, _stColumn} ok err ->
        if byteAt @k (_envBuf env) _stOff == b then
          ok b st
          { _stOff = _stOff + 1
          , _stLine = if b == asc_newline then _stLine + 1 else _stLine
          , _stColumn = if b == asc_newline then 1 else _stColumn + 1
          }
        else
          raiseError env st err $ EExpected [showByte b]
  {-# INLINE asciiByte #-}

instance Chars k => IsString (Reporter k k) where
  fromString = string
  {-# INLINE fromString #-}

raiseError :: Env k -> State -> (State -> b) -> Error -> b
raiseError env st err e = err $ addError env st e
{-# INLINE raiseError #-}

-- | Reader monad, modify environment locally
local :: (State -> Env k -> Env k) -> Reporter k a -> Reporter k a
local f p = Reporter $ \env st ok err ->
  unReporter p (f st env) st ok err
{-# INLINE local #-}

-- | Reader monad, get something from the environment
get :: (Env k -> State -> a) -> Reporter k a
get f = Reporter $ \env st ok _ -> ok (f env st) st
{-# INLINE get #-}

addLabel :: String -> Env k -> Env k
addLabel l env = case _envContext env of
  (l':_) | l == l' -> env
  ls               -> env { _envContext = take (_optMaxLabelsPerContext._envOptions $ env) $ l : ls }
{-# INLINE addLabel #-}

-- | Add parser error to the list of errors
-- which are kept in the parser state.
-- Errors of lower priority and at an earlier position.
-- Furthermore the error is merged with existing errors if possible.
addError :: Env k -> State -> Error -> State
addError env st e
  | _stOff st > _stErrOff st || _envCommit env > _stErrCommit st,
    Just e' <- mkError env e =
      st { _stErrors    = [e']
         , _stErrOff    = _stOff st
         , _stErrLine   = _stLine st
         , _stErrColumn = _stColumn st
         , _stErrCommit = _envCommit env
         }
  | _stOff st == _stErrOff st && _envCommit env == _stErrCommit st,
    Just e' <- mkError env e =
      st { _stErrors = shrinkErrors env $ e' : _stErrors st }
  | otherwise = st
{-# INLINE addError #-}

mkError :: Env k -> Error -> Maybe ErrorContext
mkError env e
  | _envHidden env, (l:ls) <- _envContext env = Just $ ErrorContext [EExpected [l]] ls
  | _envHidden env = Nothing
  | otherwise = Just $ ErrorContext [e] $ _envContext env
{-# INLINE mkError #-}

-- | Merge errors of two states, used when backtracking
mergeErrorState :: Env k -> State -> State -> State
mergeErrorState env s s'
  | _stErrOff s' > _stErrOff s || _stErrCommit s' > _stErrCommit s =
      s { _stErrors    = _stErrors s'
        , _stErrOff    = _stErrOff s'
        , _stErrLine   = _stErrLine s'
        , _stErrColumn = _stErrColumn s'
        , _stErrCommit = _stErrCommit s'
        }
  | _stErrOff s' == _stErrOff s && _stErrCommit s' == _stErrCommit s =
      s { _stErrors = shrinkErrors env $ _stErrors s' <> _stErrors s }
  | otherwise = s
{-# INLINE mergeErrorState #-}

groupOn :: Eq e => (a -> e) -> [a] -> [NonEmpty a]
groupOn f = NE.groupBy ((==) `on` f)

shrinkErrors :: Env k -> [ErrorContext] -> [ErrorContext]
shrinkErrors env = take (_optMaxContexts._envOptions $ env) . map (mergeErrorContexts env) . groupOn _ecContext . sortOn _ecContext

-- | Shrink error context by deleting duplicates
-- and merging errors if possible.
mergeErrorContexts :: Env k -> NonEmpty ErrorContext -> ErrorContext
mergeErrorContexts env es@(ErrorContext{_ecContext}:| _) = ErrorContext
  { _ecErrors = take (_optMaxErrorsPerContext._envOptions $ env) $ nubSort $ mergeEExpected $ concatMap _ecErrors $ NE.toList es
  , _ecContext = _ecContext
  }

mergeEExpected :: [Error] -> [Error]
mergeEExpected es = [EExpected $ nubSort expects | not (null expects)] <> filter (null . asEExpected) es
  where expects = concatMap asEExpected es

nubSort :: Ord a => [a] -> [a]
nubSort = map head . group . sort

asEExpected :: Error -> [String]
asEExpected (EExpected s) = s
asEExpected _ = []

defaultReportOptions :: ReportOptions
defaultReportOptions = ReportOptions
  { _optMaxContexts         = 20
  , _optMaxErrorsPerContext = 20
  , _optMaxLabelsPerContext = 5
  }

-- | Run 'Reporter' with additional 'ReportOptions'.
runReporterWithOptions :: Chunk k => ReportOptions -> Reporter k a -> FilePath -> k -> (Maybe a, [Report])
runReporterWithOptions o p f k =
  let (b, off, len) = unpackChunk k
      env = initialEnv o f b (off + len)
      ok x s = (Just x, reverse $ _stReports s)
      err s = (Nothing, reverse $ _stReports $ addReport env s)
  in unReporter p env (initialState off) ok err

-- | Run 'Reporter' on the given chunk, returning the result
-- if successful and reports from error recoveries.
-- In the case of an error, 'Nothing' is returned and the 'Report' list
-- is non-empty.
runReporter :: Chunk k => Reporter k a -> FilePath -> k -> (Maybe a, [Report])
runReporter = runReporterWithOptions defaultReportOptions

addReport :: Env k -> State -> State
addReport e s = s { _stReports = Report { _reportFile = _envFile e
                                        , _reportErrors = _stErrors s
                                        , _reportLine = _stErrLine s
                                        , _reportColumn = _stErrColumn s } : _stReports s }

initialEnv :: ReportOptions -> FilePath -> Buffer k -> Int -> Env k
initialEnv _envOptions _envFile _envBuf _envEnd = Env
  { _envFile
  , _envBuf
  , _envOptions
  , _envEnd
  , _envContext   = []
  , _envHidden    = False
  , _envCommit    = 0
  , _envRefLine   = 1
  , _envRefColumn = 1
  }

initialState :: Int -> State
initialState _stOff = State
  { _stOff
  , _stLine      = 1
  , _stColumn    = 1
  , _stErrOff    = 0
  , _stErrLine   = 0
  , _stErrColumn = 0
  , _stErrCommit = 0
  , _stErrors    = []
  , _stReports   = []
  }

-- | Pretty string representation of 'Report'.
showReport :: Report -> String
showReport r =
  "Parser errors at " <> _reportFile r
  <> ", line " <> show (_reportLine r)
  <> ", column " <> show (_reportColumn r)
  <> "\n\n" <> showErrors (_reportErrors r)

-- | Pretty string representation of '[ErrorContext]'.
showErrors :: [ErrorContext] -> String
showErrors [] = "No errors"
showErrors es = intercalate "\n" $ map showErrorContext es

showErrorContext :: ErrorContext -> String
showErrorContext ec =
  intercalate ", " (map showError $ _ecErrors ec)
  <> showContext (_ecContext ec) <> "."

showContext :: [String] -> String
showContext [] = ""
showContext xs = " in context of " <> intercalate ", " xs
