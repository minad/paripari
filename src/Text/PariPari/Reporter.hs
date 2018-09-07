{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
module Text.PariPari.Reporter (
  Reporter
  , Report(..)
  , runReporter
  , runReporterWithOptions
  , ErrorContext(..)
  , showReport
  , showErrors
  , ReportOptions(..)
  , defaultReportOptions
  , Tracer
  , runTracer
) where

import Control.Monad (void)
import Data.Function (on)
import Data.List (intercalate, sort, group, sortOn)
import Data.List.NonEmpty (NonEmpty(..))
import Data.Word (Word8)
import Debug.Trace (trace)
import GHC.Generics (Generic)
import Text.PariPari.Internal
import Text.PariPari.Class
import qualified Control.Monad.Fail as Fail
import qualified Data.List.NonEmpty as NE

data ErrorContext = ErrorContext
  { _ecErrors  :: [Error]
  , _ecContext :: [String]
  } deriving (Eq, Show, Generic)

data ReportOptions = ReportOptions
  { _optMaxContexts         :: !Int
  , _optMaxErrorsPerContext :: !Int
  , _optMaxLabelsPerContext :: !Int
  } deriving (Eq, Show, Generic)

data Report = Report
  { _reportFile   :: !FilePath
  , _reportLine   :: !Int
  , _reportCol    :: !Int
  , _reportErrors :: [ErrorContext]
  } deriving (Eq, Show, Generic)

data Env k = Env
  { _envBuf     :: !(Buffer k)
  , _envEnd     :: !Int
  , _envFile    :: !FilePath
  , _envOptions :: !ReportOptions
  , _envHidden  :: !Bool
  , _envCommit  :: !Int
  , _envContext :: [String]
  , _envRefLine :: !Int
  , _envRefCol  :: !Int
  }

data State = State
  { _stOff       :: !Int
  , _stLine      :: !Int
  , _stCol       :: !Int
  , _stErrOff    :: !Int
  , _stErrLine   :: !Int
  , _stErrCol    :: !Int
  , _stErrCommit :: !Int
  , _stErrors    :: [ErrorContext]
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

instance (Chunk k, Semigroup a) => Semigroup (Reporter k a) where
  p1 <> p2 = (<>) <$> p1 <*> p2
  {-# INLINE (<>) #-}

instance (Chunk k, Monoid a) => Monoid (Reporter k a) where
  mempty = pure mempty
  {-# INLINE mempty #-}

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
    let err' s = unReporter p2 env (mergeStateErrors env st s) ok err
    in unReporter p1 env st ok err'
  {-# INLINE (<|>) #-}

instance Chunk k => MonadPlus (Reporter k)

instance Chunk k => Monad (Reporter k) where
  p >>= f = Reporter $ \env st ok err ->
    let ok' x s = unReporter (f x) env s ok err
    in unReporter p env st ok' err
  {-# INLINE (>>=) #-}

  fail msg = Fail.fail msg
  {-# INLINE fail #-}

instance Chunk k => Fail.MonadFail (Reporter k) where
  fail msg = failWith $ EFail msg
  {-# INLINE fail #-}

instance Chunk k => ChunkParser k (Reporter k) where
  getPos = get $ \_ st -> Pos (_stLine st) (_stCol st)
  {-# INLINE getPos #-}

  getFile = get $ \env _ -> _envFile env
  {-# INLINE getFile #-}

  getRefPos = get $ \env _ -> Pos (_envRefLine env) (_envRefCol env)
  {-# INLINE getRefPos #-}

  withRefPos p = local (\st env -> env { _envRefLine = _stLine st, _envRefCol = _stCol st }) p
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
      raiseError env st err EExpectedEnd
  {-# INLINE eof #-}

  element e = Reporter $ \env st@State{_stOff, _stLine, _stCol} ok err ->
    if | _stOff >= _envEnd env -> err st
       | (e', w) <- elementAt @k (_envBuf env) _stOff, e == e',
         pos <- elementPos @k e (Pos _stLine _stCol) ->
           ok e st { _stOff =_stOff + w, _stLine = _posLine pos, _stCol = _posColumn pos }
       | otherwise ->
           raiseError env st err $ EExpected [showElement @k e]
  {-# INLINE element #-}

  elementSatisfy f = Reporter $ \env st@State{_stOff, _stLine, _stCol} ok err ->
    let (e, w) = elementAt @k (_envBuf env) _stOff
    in if | _stOff >= _envEnd env -> err st
          | f e, pos <- elementPos @k e (Pos _stLine _stCol) ->
              ok e st { _stOff =_stOff + w, _stLine = _posLine pos, _stCol = _posColumn pos }
          | otherwise ->
              raiseError env st err $ EUnexpected $ showElement @k e
  {-# INLINE elementSatisfy #-}

  chunk k = Reporter $ \env st@State{_stOff,_stCol} ok err ->
    let n = chunkWidth @k k
    in if n + _stOff <= _envEnd env &&
          chunkEqual @k (_envBuf env) _stOff k then
         ok k st { _stOff = _stOff + n, _stCol = _stCol + n }
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

instance CharChunk k => CharParser k (Reporter k) where
  satisfy f = Reporter $ \env st@State{_stOff, _stLine, _stCol} ok err ->
    let (c, w) = charAt @k (_envBuf env) _stOff
    in if | c /= '\0' ->
            if f c then
              ok c st
              { _stOff = _stOff + w
              , _stLine = if c == '\n' then _stLine + 1 else _stLine
              , _stCol = if c == '\n' then 1 else _stCol + 1
              }
            else
              raiseError env st err $ EUnexpected $ show c
          | c == '\0' && _stOff >= _envEnd env -> err st
          | otherwise -> raiseError env st err EInvalidUtf8
  {-# INLINE satisfy #-}

  -- By inling this combinator, GHC should figure out the `charWidth`
  -- of the character resulting in an optimised decoder.
  char c =
    let w = charWidth @k c
    in Reporter $ \env st@State{_stOff, _stLine, _stCol} ok err ->
      if charAtFixed @k w (_envBuf env) _stOff == c then
        ok c st
        { _stOff = _stOff + w
        , _stLine = if c == '\n' then _stLine + 1 else _stLine
        , _stCol = if c == '\n' then 1 else _stCol + 1
        }
      else
        raiseError env st err $ EExpected [show c]
  {-# INLINE char #-}

  asciiSatisfy f = Reporter $ \env st@State{_stOff, _stLine, _stCol} ok err ->
    let b = byteAt @k (_envBuf env) _stOff
        b' = fromIntegral b :: Word8
    in if | b < 128, f b' ->
              ok b' st
              { _stOff = _stOff + 1
              , _stLine = if b' == asc_newline then _stLine + 1 else _stLine
              , _stCol = if b' == asc_newline then 1 else _stCol + 1
              }
          | _stOff >= _envEnd env -> err st
          | otherwise -> raiseError env st err $ EUnexpected $ showByte b'
  {-# INLINE asciiSatisfy #-}

  asciiByte b = Reporter $ \env st@State{_stOff, _stLine, _stCol} ok err ->
      if byteAt @k (_envBuf env) _stOff == fromIntegral b then
        ok b st
        { _stOff = _stOff + 1
        , _stLine = if b == asc_newline then _stLine + 1 else _stLine
        , _stCol = if b == asc_newline then 1 else _stCol + 1
        }
      else
        raiseError env st err $ EExpected [showByte b]
  {-# INLINE asciiByte #-}

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
         , _stErrCol    = _stCol st
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
mergeStateErrors :: Env k -> State -> State -> State
mergeStateErrors env s s'
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
{-# INLINE mergeStateErrors #-}

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
runReporterWithOptions :: Chunk k => ReportOptions -> Reporter k a -> FilePath -> k -> Either Report a
runReporterWithOptions o p f k =
  let (b, off, len) = unpackChunk k
  in unReporter p (initialEnv o f b (off + len)) (initialState off) (\x _ -> Right x) (Left . getReport f)

-- | Run 'Reporter' on the given 'ByteString', returning either
-- an error 'Report' or, if successful, the result.
runReporter :: Chunk k => Reporter k a -> FilePath -> k -> Either Report a
runReporter = runReporterWithOptions defaultReportOptions

getReport :: FilePath -> State -> Report
getReport f s = Report f (_stErrLine s) (_stErrCol s) (_stErrors s)

initialEnv :: ReportOptions -> FilePath -> Buffer k -> Int -> Env k
initialEnv _envOptions _envFile _envBuf _envEnd = Env
  { _envFile
  , _envBuf
  , _envOptions
  , _envEnd
  , _envContext = []
  , _envHidden  = False
  , _envCommit  = 0
  , _envRefLine = 0
  , _envRefCol  = 0
  }

initialState :: Int -> State
initialState _stOff = State
  { _stOff
  , _stLine      = 1
  , _stCol       = 1
  , _stErrOff    = 0
  , _stErrLine   = 0
  , _stErrCol    = 0
  , _stErrCommit = 0
  , _stErrors    = []
  }

-- | Run 'Tracer' on the given 'ByteString', returning either
-- an error 'Report' or, if successful, the result.
runTracer :: Chunk k => Tracer k a -> FilePath -> k -> Either Report a
runTracer = runReporter . unTracer

-- | Pretty string representation of 'Report'.
showReport :: Report -> String
showReport r =
  "Parser errors at " <> _reportFile r
  <> ", line " <> show (_reportLine r)
  <> ", column " <> show (_reportCol r)
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
              next  = unReporter (unTracer p2) env (mergeStateErrors env st s) ok err
          in if width > 1 then
               trace ("Back tracking " <> show width <> " bytes at line " <> show (_stLine s)
                       <> ", column " <> show (_stCol s) <> ", context " <> show (_envContext env) <> ": "
                       <> showChunk (packChunk @k (_envBuf env) (_stOff st) width)) next
             else
               next
    in unReporter (unTracer p1) env st ok err'
