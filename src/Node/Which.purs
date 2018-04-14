module Node.Which (which, which') where

import Prelude

import Control.Monad.Aff (Aff, makeAff, nonCanceler)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (Error)
import Data.Either (Either(..))
import Data.Maybe (Maybe(Nothing))
import Data.Nullable (toNullable, Nullable)
import Node.FS (FS)

foreign import whichImpl :: forall eff.
  { path :: Nullable String, pathExt :: Nullable String} ->
  String ->
  (Error -> Eff (fs :: FS | eff) Unit) ->
  ((Array String) -> Eff (fs :: FS | eff) Unit) ->
  Eff (fs :: FS | eff) Unit

which :: forall eff. String -> Aff (fs :: FS | eff) (Array String)
which s = makeAff \ cb -> nonCanceler <$
  whichImpl { path: toNullable Nothing, pathExt: toNullable Nothing } s (cb <<< Left) (cb <<< Right)

which' :: forall eff. { path :: Maybe String, pathExt :: Maybe String } -> String -> Aff (fs :: FS | eff) (Array String)
which' { path, pathExt } s = makeAff \ cb -> nonCanceler <$
  whichImpl { path: toNullable path, pathExt: toNullable pathExt } s (cb <<< Left) (cb <<< Right)
