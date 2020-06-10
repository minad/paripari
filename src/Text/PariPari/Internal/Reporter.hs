{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE MagicHash #-}
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
import Data.Semigroup as Sem
import Data.String (IsString(..))
import GHC.Base
import GHC.Word
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
  , _reportCol :: {-#UNPACK#-}!Int
  } deriving (Eq, Show, Generic)

data Env k = Env
  { _envBuf       :: !(Buffer k)
  , _envFile      :: !FilePath
  , _envOptions   :: !ReportOptions
  , _envHidden    :: !Bool
  , _envContext   :: [String]
  , _envRefLine   :: {-#UNPACK#-}!Int
  , _envRefCol :: {-#UNPACK#-}!Int
  }

data State = State
  { _stOff       :: Int#
  , _stLine      :: {-#UNPACK#-}!Int
  , _stCol    :: {-#UNPACK#-}!Int
  , _stErrOff    :: {-#UNPACK#-}!Int
  , _stErrLine   :: {-#UNPACK#-}!Int
  , _stErrCol :: {-#UNPACK#-}!Int
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

instance Chunk k => Parser k (Reporter k) where
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

  try p = Reporter $ \env st ok err ->
    let err' _ = err st
    in unReporter p env st ok err'
  {-# INLINE try #-}

  p1 <!> p2 = Reporter $ \env st ok err ->
    let err' s
          | 1# <- _stOff s ==# _stOff st = unReporter p2 env (mergeErrorState env st s) ok err
          | otherwise = err s
    in unReporter p1 env st ok err'
  {-# INLINE (<!>) #-}

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
    case indexByte @k (_envBuf env) (_stOff st) `eqWord#` int2Word# 0# of
      1# -> ok () st
      _ -> raiseError env st err expectedEnd
  {-# INLINE eof #-}

  recover p r = Reporter $ \env st ok err ->
    let err1 s =
          let err2 _ = err s
          in unReporter r env (addReport env s) ok err2
    in unReporter p env st ok err1
  {-# INLINE recover #-}

  chunk k = Reporter $ \env st@State{_stOff,_stCol} ok err ->
    case matchChunk @k (_envBuf env) _stOff k of
      -1# -> raiseError env st err $ EExpected [showChunk @k k]
      n -> ok k st { _stOff = _stOff +# n, _stCol = _stCol + I# n }
  {-# INLINE chunk #-}

  asChunk p = do
    I# begin' <- get (const (\s -> I# (_stOff s)))
    p
    I# end' <- get (const (\s -> I# (_stOff s)))
    src <- get (\env _ -> _envBuf env)
    pure $ packChunk src begin' (end' -# begin')
  {-# INLINE asChunk #-}

  scan f = Reporter $ \env st@State{_stOff, _stLine, _stCol} ok err ->
    case indexChar @k (_envBuf env) _stOff of
      (# '\0'#, _ #) -> raiseError env st err unexpectedEnd
      (# c, w #) ->
        case f (C# c) of
          Just r ->
            ok r st { _stOff = _stOff +# w
                    , _stLine = case c `eqChar#` '\n'# of 1# -> _stLine + 1; _ -> _stLine
                    , _stCol = case c `eqChar#` '\n'# of 1# -> 1; _ -> _stCol + 1
                    }
          Nothing -> raiseError env st err $ EUnexpected $ show (C# c)
  {-# INLINE scan #-}

  -- By inling this combinator, GHC should figure out the `charWidth`
  -- of the character resulting in an optimised decoder.
  char '\0' = error "Character '\\0' cannot be parsed because it is used as sentinel"
  char c@(C# c') =
    Reporter $ \env st@State{_stOff, _stLine, _stCol} ok err ->
        case matchChar @k (_envBuf env) _stOff c' of
          -1# -> raiseError env st err $ EExpected [show c]
          w -> ok c st
            { _stOff = _stOff +# w
            , _stLine = if c == '\n' then _stLine + 1 else _stLine
            , _stCol = if c == '\n' then 1 else _stCol + 1
            }
  {-# INLINE char #-}

  asciiScan f = Reporter $ \env st@State{_stOff, _stLine, _stCol} ok err ->
    let b = W8# (indexByte @k (_envBuf env) _stOff)
    in if | b /= 0,
            b < 128,
            Just x <- f b ->
              ok x st
              { _stOff = _stOff +# 1#
              , _stLine = if b == asc_newline then _stLine + 1 else _stLine
              , _stCol = if b == asc_newline then 1 else _stCol + 1
              }
          | otherwise ->
              raiseError env st err $ EUnexpected $ showByte b
  {-# INLINE asciiScan #-}

  asciiByte 0 = error "Character '\\0' cannot be parsed because it is used as sentinel"
  asciiByte b
    | b >= 128 = error "Not an ASCII character"
    | otherwise = Reporter $ \env st@State{_stOff, _stLine, _stCol} ok err ->
        if W8# (indexByte @k (_envBuf env) _stOff) == b then
          ok b st
          { _stOff = _stOff +# 1#
          , _stLine = if b == asc_newline then _stLine + 1 else _stLine
          , _stCol = if b == asc_newline then 1 else _stCol + 1
          }
        else
          raiseError env st err $ EExpected [showByte b]
  {-# INLINE asciiByte #-}

instance Chunk k => IsString (Reporter k k) where
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
  | I# (_stOff st) > _stErrOff st,
    Just e' <- mkError env e =
      st { _stErrors    = [e']
         , _stErrOff    = I# (_stOff st)
         , _stErrLine   = _stLine st
         , _stErrCol = _stCol st
         }
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
  | _stErrOff s' > _stErrOff s =
      s { _stErrors    = _stErrors s'
        , _stErrOff    = _stErrOff s'
        , _stErrLine   = _stErrLine s'
        , _stErrCol = _stErrCol s'
        }
  | _stErrOff s' == _stErrOff s =
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
  let !(# b, off #) = unpackChunk k
      env = initialEnv o f b
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
                                        , _reportCol = _stErrCol s } : _stReports s }

initialEnv :: ReportOptions -> FilePath -> Buffer k -> Env k
initialEnv _envOptions _envFile _envBuf = Env
  { _envFile
  , _envBuf
  , _envOptions
  , _envContext   = []
  , _envHidden    = False
  , _envRefLine   = 1
  , _envRefCol = 1
  }

initialState :: Int# -> State
initialState _stOff = State
  { _stOff
  , _stLine      = 1
  , _stCol    = 1
  , _stErrOff    = 0
  , _stErrLine   = 0
  , _stErrCol = 0
  , _stErrors    = []
  , _stReports   = []
  }

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
