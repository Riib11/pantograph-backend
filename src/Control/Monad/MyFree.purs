-- | Not sure why I wrote this, just felt compelled.
module Control.Monad.MyFree where

import Prelude

data Free (f :: Type -> Type) (a :: Type)
  = Pure a
  | Free (f (Free f a))

showFree :: forall f a. Functor f => (a -> String) -> (f String -> String) -> Free f a -> String
showFree show_a _show_f (Pure a) = show_a a

showFree show_a show_f (Free e) = show_f (showFree show_a show_f <$> e)

instance functorFree :: Functor f => Functor (Free f) where
  map f (Pure a) = Pure (f a)
  map f (Free e) = Free ((f <$> _) <$> e)

instance applyFree :: Functor f => Apply (Free f) where
  apply (Pure f) fa = f <$> fa
  apply (Free ef) fa = Free (ef <#> (_ <*> fa))

instance applicativeFree :: Functor f => Applicative (Free f) where
  pure = Pure

instance bindFree :: Functor f => Bind (Free f) where
  bind (Pure a) k = k a
  bind (Free e) k = Free (e <#> (_ >>= k))

instance monadFree :: Functor f => Monad (Free f)
