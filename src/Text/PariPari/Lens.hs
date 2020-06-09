{-# LANGUAGE Rank2Types #-}
module Text.PariPari.Lens (
  posLine
  , posColumn
  , reportLine
  , reportColumn
  , reportFile
  , reportErrors
  , ecErrors
  , ecContext
  , optMaxContexts
  , optMaxErrorsPerContext
  , optMaxLabelsPerContext
) where

import Text.PariPari.Internal.Class
import Text.PariPari.Internal.Reporter

type Lens a b = forall f . Functor f => (b -> f b) -> (a -> f a)

posLine :: Lens Pos Int
posLine k p = fmap (\x -> p { _posLine = x }) (k (_posLine p))
{-# INLINE posLine #-}

posColumn :: Lens Pos Int
posColumn k p = fmap (\x -> p { _posColumn = x }) (k (_posColumn p))
{-# INLINE posColumn #-}

reportLine :: Lens Report Int
reportLine k r = fmap (\x -> r { _reportLine = x }) (k (_reportLine r))
{-# INLINE reportLine #-}

reportColumn :: Lens Report Int
reportColumn k r = fmap (\x -> r { _reportColumn = x }) (k (_reportColumn r))
{-# INLINE reportColumn #-}

reportFile :: Lens Report FilePath
reportFile k r = fmap (\x -> r { _reportFile = x }) (k (_reportFile r))
{-# INLINE reportFile #-}

reportErrors :: Lens Report [ErrorContext]
reportErrors k r = fmap (\x -> r { _reportErrors = x }) (k (_reportErrors r))
{-# INLINE reportErrors #-}

ecErrors :: Lens ErrorContext [Error]
ecErrors k e = fmap (\x -> e { _ecErrors = x }) (k (_ecErrors e))
{-# INLINE ecErrors #-}

ecContext :: Lens ErrorContext [String]
ecContext k e = fmap (\x -> e { _ecContext = x }) (k (_ecContext e))
{-# INLINE ecContext #-}

optMaxContexts :: Lens ReportOptions Int
optMaxContexts k o = fmap (\x -> o { _optMaxContexts = x }) (k (_optMaxContexts o))
{-# INLINE optMaxContexts #-}

optMaxErrorsPerContext :: Lens ReportOptions Int
optMaxErrorsPerContext k o = fmap (\x -> o { _optMaxErrorsPerContext = x }) (k (_optMaxErrorsPerContext o))
{-# INLINE optMaxErrorsPerContext #-}

optMaxLabelsPerContext :: Lens ReportOptions Int
optMaxLabelsPerContext k o = fmap (\x -> o { _optMaxLabelsPerContext = x }) (k (_optMaxLabelsPerContext o))
{-# INLINE optMaxLabelsPerContext #-}
