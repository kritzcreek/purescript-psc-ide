module Node.Which (which, which') where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(Nothing))
import Data.Nullable (toNullable, Nullable)
import Effect (Effect)
import Effect.Aff (Aff, Error, makeAff, nonCanceler)

foreign import whichImpl ::
  { path :: Nullable String, pathExt :: Nullable String} ->
  String ->
  (Error -> Effect Unit) ->
  ((Array String) -> Effect Unit) ->
  Effect Unit

which :: String -> Aff (Array String)
which s = makeAff \ cb -> nonCanceler <$
  whichImpl { path: toNullable Nothing, pathExt: toNullable Nothing } s (cb <<< Left) (cb <<< Right)

which' :: { path :: Maybe String, pathExt :: Maybe String } -> String -> Aff (Array String)
which' { path, pathExt } s = makeAff \ cb -> nonCanceler <$
  whichImpl { path: toNullable path, pathExt: toNullable pathExt } s (cb <<< Left) (cb <<< Right)
