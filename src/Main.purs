module Main where

import Prelude
import Data.Maybe (Maybe(..))
import Data.Zipr (mapZipr)
import Effect (Effect)
import Effect.Console (log)
import Language.Pantograph.Grammar (TypeTooth(..), TypeZiprTooth(..), arr, arr1, arr2, base, showType, showsTypePath)

main :: Effect Unit
main = do
  -- 
  let
    type0 = base 0

    type1 = base 1
  log $ "type0 = " <> showType type0
  log $ "type1 = " <> showType type1
  -- 
  let
    type2 = arr type0 type1
  log $ "type2 = " <> showType type2
  let
    path2 = arr1 type0 <<< Just $ arr2 type1 <<< Just $ arr1 type2 $ Nothing
  log $ "path2 = " <> showsTypePath path2 "{{}}"
  -- 
  let
    f =
      mapZipr
        ( case _ of
            (TypeTooth expr (Base a)) -> (TypeTooth expr (Base (a + 1)))
            th -> th
        )
  -- 
  log $ "f type2 = " <> showType (f type2)
  log $ "f path2 = " <> showsTypePath (f path2) "{{}}"
