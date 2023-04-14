module Data.Logic (Top, Bot, Not, introDoubleNot) where

import Prelude

type Top
  = Unit

type Bot
  = Not Top

type Not a
  = forall x. a -> x

introDoubleNot :: forall a. a -> Not (Not a)
introDoubleNot a = \na -> na a
