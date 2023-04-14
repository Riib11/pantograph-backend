module Data.Zipr where

import Prelude
import Data.Foldable (class Foldable)
import Data.Maybe (Maybe, maybe)
import Text.Shows (class Shows, ViaShows(..), shows)

data Zipr :: (Type -> Type -> Type -> Type -> Type) -> Type -> Type -> Type -> Type
data Zipr th path expr a
  = Path path (th path expr a (Zipr th expr path a)) (Maybe (Zipr th path expr a))
  | Expr expr (th path expr a (Zipr th path expr a))

type Path :: (Type -> Type -> Type -> Type -> Type) -> Type -> Type
type Path th a
  = Zipr th Unit Void a

type Path' :: (Type -> Type -> Type -> Type -> Type) -> Type -> Type -> Type
type Path' th a
  = th Unit Void a

type Expr :: (Type -> Type -> Type -> Type -> Type) -> Type -> Type
type Expr th a
  = Zipr th Void Unit a

type Expr' :: (Type -> Type -> Type -> Type -> Type) -> Type -> Type -> Type
type Expr' th a
  = th Void Unit a

path :: forall th expr a. th Unit expr a (Zipr th expr Unit a) -> Maybe (Zipr th Unit expr a) -> Zipr th Unit expr a
path = Path unit

expr :: forall th path a. th path Unit a (Zipr th path Unit a) -> Zipr th path Unit a
expr = Expr unit

showsPath ::
  forall th a.
  Functor (Path' th a) =>
  Functor (Expr' th a) =>
  (Path' th a String -> String -> String) ->
  (Expr' th a String -> String) ->
  Path th a -> String -> String
showsPath showPath' showExpr' (Path _ th mb_z) =
  showPath'
    (showExpr showExpr' <$> th)
    >>> maybe identity (showsPath showPath' showExpr') mb_z

showsPath _ _ (Expr v _) = absurd v

showExpr ::
  forall th a.
  Functor (Expr' th a) =>
  (Expr' th a String -> String) ->
  Expr th a -> String
showExpr _ (Path v _ _) = absurd v

showExpr showExpr' (Expr _ th) = showExpr' (showExpr showExpr' <$> th)

mapZipr ::
  forall th a.
  (forall path' expr' r'. Functor (th path' expr' a) => th path' expr' a r' -> th path' expr' a r') ->
  forall path expr.
  Functor (th path expr a) =>
  Functor (th expr path a) =>
  Zipr th path expr a ->
  Zipr th path expr a
mapZipr f (Path w th z) = Path w (f <<< (mapZipr f <$> _) $ th) (mapZipr f <$> z)

mapZipr f (Expr w th) = Expr w (f <<< (mapZipr f <$> _) $ th)

foldMapZipr ::
  forall th a m.
  Monoid m =>
  ( forall path' expr'.
    Functor (th path' expr' a) =>
    Foldable (th path' expr' a) =>
    th path' expr' a m -> m
  ) ->
  forall path expr.
  Functor (th path expr a) =>
  Functor (th expr path a) =>
  Foldable (th path expr a) =>
  Foldable (th expr path a) =>
  Zipr th path expr a ->
  m
foldMapZipr f (Path _ th z) = f (foldMapZipr f <$> th) <> maybe mempty (foldMapZipr f) z

foldMapZipr f (Expr _ th) = f (foldMapZipr f <$> th)
