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

-- | Expr (th a Void (Zipr th a Void))
-- | The type of trees-that-might-contain-a-hole (i.e. ztree) over a tooth type.
-- | - `th` is the type of teeth
-- | - `a` is the type of base cases
-- | - `h` is the type of holes in this ztree
-- |   - `h = Unit` indicates that there is ONE hole in this ztree 
-- |   - `h = Void` indicates that there are NO holes in this ztree
{-
-- | The type of zippers over a tooth functor `th a h`. Expects that `th` has
-- | proper tooth structure where `h` is used exactly zero or one times in each
-- | constructor.
newtype Zipr (th :: Type -> Type -> Type -> Type) a h
  = Zipr (th a h (Zipr th a h))

-- | Zipr unfolded once.
type Zipr' th a h
  = th a h (Zipr th a h)

instance showZipr :: (Bifunctor (th a), Show (th a h String)) => Show (Zipr th a h) where
  show = foldMapZipr show

derive instance newtypeZipr :: Newtype (Zipr th a h) _

-- | Map over every tooth of zipper.
mapZipr ∷ ∀ th a h1 h2. Bifunctor (th a) ⇒ (∀ b. th a h1 b → th a h2 b) → Zipr th a h1 → Zipr th a h2
mapZipr f (Zipr th) = Zipr $ f $ rmap (mapZipr f) $ th

-- | Map over zipper hole.
mapZiprHole :: forall th a h1 h2. Bifunctor (th a) => (h1 -> h2) -> Zipr th a h1 -> Zipr th a h2
mapZiprHole f (Zipr th) = Zipr $ bimap f (mapZiprHole f) th

-- | Fold map over every tooth of zipper.
foldMapZipr :: forall th a h m. Bifunctor (th a) => Monoid m => (th a h m -> m) -> Zipr th a h -> m
foldMapZipr f (Zipr th) = f $ rmap (foldMapZipr f) $ th

zip ∷ ∀ th a h. th a h (Zipr th a h) → Zipr th a h
zip = wrap

unzip ∷ ∀ th a h. Zipr th a h → th a h (Zipr th a h)
unzip = unwrap

overZipr :: forall th1 a1 h1 th2 a2 h2. (th1 a1 h1 (Zipr th1 a1 h1) -> th2 a2 h2 (Zipr th2 a2 h2)) -> Zipr th1 a1 h1 -> Zipr th2 a2 h2
overZipr = over Zipr

-- | The type of expressions over a tooth functor `th a h`, defined in terms of
-- | zippers. An expression is a zipper with an expression in its h.
type Expr th a
  = Zipr th a Void
-}
-- -- | The type of expressions over a tooth functor `th a h`, defined in terms of zippers.
-- -- | An expression is a zipper with an expression in its h.
-- newtype Expr th a
--   = Expr (Zipr th a (Expr th a))
-- derive instance newtypeExpr :: Newtype (Expr th a) _
-- expr :: forall th a. th a (Expr th a) (Zipr th a (Expr th a)) -> Expr th a
-- expr = wrap <<< wrap
-- unexpr :: forall th a. Expr th a -> th a (Expr th a) (Zipr th a (Expr th a))
-- unexpr = unwrap <<< unwrap
-- overExpr :: forall th1 a1 th2 a2. (th1 a1 (Expr th1 a1) (Zipr th1 a1 (Expr th1 a1)) -> th2 a2 (Expr th2 a2) (Zipr th2 a2 (Expr th2 a2))) -> Expr th1 a1 -> Expr th2 a2
-- overExpr f = over Expr (over Zipr f)
-- mapExpr ∷ ∀ th a. Bifunctor (th a) ⇒ (∀ h b. th a h b → th a h b) → Expr th a → Expr th a
-- mapExpr f (Expr (Zipr th)) = Expr $ Zipr $ bimap (mapExpr f) (mapZipr f) th
-- foldMapExpr :: forall th a m. Bifunctor (th a) => Monoid m => (th a m m -> m) -> Expr th a -> m
-- foldMapExpr f (Expr (Zipr th)) = f $ bimap (foldMapExpr f) (foldMapZipr f <<< mapZipr (lmap (foldMapExpr f))) $ th
