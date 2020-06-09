{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}

import Data.Foldable (for_)
import System.Environment (getArgs)
import Text.PariPari
import qualified Data.Char as C
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T
import qualified Data.Text.IO as T

type StringType = T.Text
type PMonad p = Parser StringType p
type P a = (forall p. PMonad p => p a)

data Type
  = TypeName   !StringType
  | TypeVar    !StringType
  | TypeApp    !Type !(NE.NonEmpty Type)
  | TypeEq     !Type !Type
  | TypeConstr !Type !Type
  | TypeLam    !Type !Type
  | TypeTuple  ![Type]
  | TypeList   !Type
  deriving (Show, Eq)

data SourceLine
  = SpecialiseAll !Type !Type
  | TypeDecl      !(NE.NonEmpty StringType) !Type
  | OtherLine     !StringType
  deriving (Show)

source :: P [SourceLine]
source = sepBy sourceLine (char '\n') <* eof

sourceLine :: P SourceLine
sourceLine = specialiseAll <|> typeDecl <|> otherLine

otherLine :: P SourceLine
otherLine = OtherLine <$> takeCharsWhile (/= '\n')

specialiseAll :: P SourceLine
specialiseAll = SpecialiseAll
  <$> (symbol "{-#" *> symbol "SPECIALISE_ALL" *> type_)
  <*> (symbol "=" *> type_ <* symbol "#-}")

identifierAtom :: PMonad p => (Char -> Bool) -> p ()
identifierAtom f = satisfy f *> skipCharsWhile (\c -> C.isAlphaNum c || c == '_' || c == '\'')

name :: P StringType
name = asChunk (sepEndBy (identifierAtom C.isUpper) (char '.') *>
                identifierAtom C.isLower) <* space

typeName :: P StringType
typeName = asChunk (void $ sepBy1 (identifierAtom C.isUpper) (char '.')) <* space

symbol :: PMonad p => String -> p StringType
symbol s = string s <* space

typeTuple :: P Type
typeTuple = do
  ts <- between (symbol "(") (symbol ")") (sepBy type_ (symbol ","))
  pure $ case ts of
           []  -> TypeName "()"
           [t] -> t
           _   -> TypeTuple ts

typeAtom :: P Type
typeAtom =
  TypeName <$> typeName
  <|> TypeVar <$> name
  <|> TypeList <$> between (symbol "[") (symbol "]") type_
  <|> typeTuple

typeApp :: P Type
typeApp = do
  t <- typeAtom
  option t $ TypeApp t <$> some typeAtom

type_ :: P Type
type_ = do
  t <- typeApp
  TypeEq t <$> (symbol "~" *> type_)
    <|> TypeLam t <$> (symbol "->" *> type_)
    <|> TypeConstr t <$> (symbol "=>" *> type_)
    <|> pure t

typeDecl :: P SourceLine
typeDecl = TypeDecl <$> sepBy1 name (symbol ",") <*> (symbol "::" *> type_)

space :: P ()
space = skipCharsWhile (== ' ')

showType :: Type -> StringType
showType (TypeName t)  = t
showType (TypeVar t)   = t
showType (TypeApp t a) = "(" <> showType t <> " " <> T.intercalate " " (NE.toList $ fmap showType a) <> ")"
showType (TypeEq a b)  = "(" <> showType a <> " ~ " <> showType b <> ")"
showType (TypeLam a b)  = "(" <> showType a <> " -> " <> showType b <> ")"
showType (TypeConstr a b)  = "(" <> showType a <> " => " <> showType b <> ")"
showType (TypeList t) = "[" <> showType t <> "]"
showType (TypeTuple t) = "(" <> T.intercalate ", " (fmap showType t) <> ")"

showSource :: SourceLine -> StringType
showSource (SpecialiseAll from to) = "-- SPECIALISE_ALL " <> showType from <> " = " <> showType to
showSource (TypeDecl      names t) = T.intercalate "," (NE.toList names) <> " :: " <> showType t
showSource (OtherLine     str)     = str

substitute :: [(StringType, Type)] -> Type -> Type
substitute vars t@(TypeVar v)
  | Just t' <- lookup v vars = t'
  | otherwise = t
substitute _    t@TypeName{} = t
substitute vars (TypeApp t a) = TypeApp (substitute vars t) (fmap (substitute vars) a)
substitute vars (TypeEq a b) = TypeEq (substitute vars a) (substitute vars b)
substitute vars (TypeConstr a b) = TypeConstr (substitute vars a) (substitute vars b)
substitute vars (TypeLam a b) = TypeLam (substitute vars a) (substitute vars b)
substitute vars (TypeTuple t) = TypeTuple (fmap (substitute vars) t)
substitute vars (TypeList t) = TypeList (substitute vars t)

unify :: Type -> Type -> Maybe [(StringType, Type)]
unify (TypeVar v) t =
  Just [(v, t)]
unify (TypeName t1) (TypeName t2)
  | t1 == t2 = Just []
unify (TypeApp t1 a1) (TypeApp t2 a2) | length a1 == length a2 = do
  t <- unify t1 t2
  a <- foldMap (uncurry unify) $ NE.zip a1 a2
  pure $ t <> a
unify (TypeEq a1 b1) (TypeEq a2 b2) = do
  a <- unify a1 a2
  b <- unify b1 b2
  pure $ a <> b
unify (TypeConstr a1 b1) (TypeConstr a2 b2) = do
  a <- unify a1 a2
  b <- unify b1 b2
  pure $ a <> b
unify (TypeLam a1 b1) (TypeLam a2 b2) = do
  a <- unify a1 a2
  b <- unify b1 b2
  pure $ a <> b
unify (TypeTuple t1) (TypeTuple t2) | length t1 == length t2 =
  foldMap (uncurry unify) $ zip t1 t2
unify (TypeList t1) (TypeList t2) =
  unify t1 t2
unify _ _ = Nothing

simplify :: Type -> Type
simplify t@TypeVar{} = t
simplify t@TypeName{} = t
simplify (TypeApp t a) = TypeApp (simplify t) (fmap simplify a)
simplify (TypeEq a b) = TypeEq (simplify a) (simplify b)
simplify (TypeLam a b) = TypeLam (simplify a) (simplify b)
simplify (TypeTuple t) = TypeTuple (fmap simplify t)
simplify (TypeList t) = TypeList (simplify t)
simplify (TypeConstr (TypeEq (TypeVar v) t) t') = substitute [(v, t)] t'
simplify (TypeConstr c t) = TypeConstr (TypeTuple otherConstraints) (substitute constraintVars t)
  where eqConstraints = [e | e@(TypeEq TypeVar{} _) <- constraints]
        constraintVars = [(v,x) | TypeEq (TypeVar v) x <- eqConstraints]
        otherConstraints = filter (\x -> all (/= x) eqConstraints) constraints
        constraints = case c of
          TypeTuple xs -> xs
          x -> [x]

specialiseType :: Type -> Type -> Type -> Type
specialiseType from to typ
  | Just vars <- unify from typ = substitute vars to
  | otherwise =
      case typ of
        TypeName{}     -> typ
        TypeVar{}      -> typ
        TypeApp t a    -> TypeApp (specialiseType from to t) (fmap (specialiseType from to) a)
        TypeEq a b     -> TypeEq (specialiseType from to a) (specialiseType from to b)
        TypeConstr a b -> TypeConstr (specialiseType from to a) (specialiseType from to b)
        TypeLam a b    -> TypeLam (specialiseType from to a) (specialiseType from to b)
        TypeTuple t    -> TypeTuple (fmap (specialiseType from to) t)
        TypeList t     -> TypeList (specialiseType from to t)

specialise :: [(Type, Type)] -> (NE.NonEmpty StringType, Type) -> [StringType]
specialise specs (names, typ) = concatMap go specs
  where go (from, to)
          | typ' <- specialiseType from to typ, typ /= typ' =
              ["{-# SPECIALISE " <> n <> " :: " <> showType (simplify typ') <> " #-}" | n <- NE.toList names]
          | otherwise = []

main :: IO ()
main = do
  args <- getArgs
  case args of
    [src, _, dst] -> do
      code <- T.readFile src
      let (result, reports) = runParser source src code
      for_ reports $ putStrLn . showReport
      case result of
        Nothing -> pure ()
        Just ls -> do
          let specialisers = [(from, to) | SpecialiseAll from to <- ls]
              specialisedTypeDecls = concatMap (specialise specialisers) [(n, t) | TypeDecl n t <- ls]
          T.writeFile dst $ T.intercalate "\n" $ map showSource ls <> specialisedTypeDecls
    _ -> error "Usage: paripari-specialise-all src _ dst"
