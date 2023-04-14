module Type.ClassicalLogic where

import Prelude

class Eval (a :: Type) (b :: Type) | a -> b

class Interp (a :: Type) (b :: Type) | a -> b

data Top

instance interpTop :: Interp Top Unit

data Bot

instance interpBot :: Interp Bot Void

data Not (a :: Type)

instance evalNotTop :: Eval (Not Top) Bot

instance evalNotBot :: Eval (Not Bot) Top
