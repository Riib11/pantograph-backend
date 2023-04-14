module Language.Pantograph.Grammar where

import Prelude
import Prim hiding (Type)
import Data.Foldable (class Foldable)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe)
import Data.Traversable (class Traversable)
import Data.Zipr (Expr, Expr', Path, Path', Zipr(..), expr, path, showExpr, showsPath)
import Text.Shows (class Shows, shows)

data TypeZiprTooth :: Prim.Type -> Prim.Type -> Prim.Type -> Prim.Type -> Prim.Type
data TypeZiprTooth path expr a rec
  = TypePathTooth path (TypePathTooth a rec)
  | TypeTooth expr (TypeTooth a rec)

data TypePathTooth :: Prim.Type -> Prim.Type -> Prim.Type
data TypePathTooth a rec
  = Arr1 rec
  | Arr2 rec

data TypeTooth :: Prim.Type -> Prim.Type -> Prim.Type
data TypeTooth a rec
  = Base a
  | Arr rec rec

derive instance genericTypeZiprTooth :: Generic (TypeZiprTooth path expr a rec) _

derive instance functorTypeZiprTooth :: Functor (TypeZiprTooth path expr a)

derive instance foldableTypeZiprTooth :: Foldable (TypeZiprTooth path expr a)

derive instance traversableTypeZiprTooth :: Traversable (TypeZiprTooth path expr a)

derive instance genericTypePathTooth :: Generic (TypePathTooth a rec) _

derive instance functorTypePathTooth :: Functor (TypePathTooth a)

derive instance foldableTypePathTooth :: Foldable (TypePathTooth a)

derive instance traversableTypePathTooth :: Traversable (TypePathTooth a)

derive instance genericTypeTooth :: Generic (TypeTooth a rec) _

derive instance functorTypeTooth :: Functor (TypeTooth a)

derive instance foldableTypeTooth :: Foldable (TypeTooth a)

derive instance traversableTypeTooth :: Traversable (TypeTooth a)

type TypePath a
  = Path TypeZiprTooth a

type TypePath' a
  = Path' TypeZiprTooth a

type Type a
  = Expr TypeZiprTooth a

type Type' a
  = Expr' TypeZiprTooth a

matchTypePath ::
  forall a x.
  TypePath a ->
  { arr1 :: Type a -> Maybe (TypePath a) -> x
  , arr2 :: Type a -> Maybe (TypePath a) -> x
  } ->
  x
matchTypePath (Expr v _) _ = absurd v

matchTypePath (Path _ (TypeTooth v _) p) _ = absurd v

matchTypePath (Path _ (TypePathTooth _ (Arr1 cod)) mb_p) rec = rec.arr1 cod mb_p

matchTypePath (Path _ (TypePathTooth _ (Arr2 dom)) mb_p) rec = rec.arr2 dom mb_p

matchType ::
  forall a x.
  Type a ->
  { base :: a -> x
  , arr :: Type a -> Type a -> x
  } ->
  x
matchType (Path v _ _) _ = absurd v

matchType (Expr _ (TypePathTooth v _)) _ = absurd v

matchType (Expr _ (TypeTooth _ (Base a))) rec = rec.base a

matchType (Expr _ (TypeTooth _ (Arr dom cod))) rec = rec.arr dom cod

instance showsTypeZiprToothPathString :: Shows (TypePath' a String) where
  shows (TypeTooth v _) = absurd v
  shows (TypePathTooth _ (Arr1 cod)) = \dom_str -> "(" <> dom_str <> " -> " <> cod <> ")"
  shows (TypePathTooth _ (Arr2 dom)) = \cod_str -> "(" <> dom <> " -> " <> cod_str <> ")"

showsTypePath :: forall a. Show a => Functor (TypePath' a) => TypePath a -> String -> String
showsTypePath = showsPath shows show

instance showTypeZiprToothString :: Show a => Show (Type' a String) where
  show (TypePathTooth v _) = absurd v
  show (TypeTooth _ (Base a)) = show a
  show (TypeTooth _ (Arr dom cod)) = "(" <> dom <> " -> " <> cod <> ")"

showType :: forall a. Show a => Type a -> String
showType = showExpr show

-- | expressions
base :: forall a. a -> Type a
base a = expr $ TypeTooth unit $ Base a

arr :: forall a. Type a -> Type a -> Type a
arr dom cod = expr $ TypeTooth unit $ Arr dom cod

-- | paths 
arr1 :: forall a. Type a -> Maybe (TypePath a) -> TypePath a
arr1 dom = path $ TypePathTooth unit $ Arr1 dom

arr2 :: forall a. Type a -> Maybe (TypePath a) -> TypePath a
arr2 dom = path $ TypePathTooth unit $ Arr2 dom
