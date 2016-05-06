module Node.Which (which) where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Aff (Aff, makeAff)
import Control.Monad.Eff.Exception (Error)
import Node.FS (FS)

foreign import whichImpl :: forall eff.
  String ->
  (Error -> Eff (fs :: FS | eff) Unit) ->
  ((Array String) -> Eff (fs :: FS | eff) Unit) ->
  Eff (fs :: FS | eff) Unit

which :: forall eff. String -> Aff (fs :: FS | eff) (Array String)
which = makeAff <<< whichImpl
