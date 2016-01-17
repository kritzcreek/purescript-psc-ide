module PscIde where

import Control.Monad.Eff (Eff)
import Control.Monad.Aff (Aff, makeAff)
import Control.Monad.Eff.Exception (Error)
import Control.Alt ((<|>))
import Prelude (Unit, (>>>), show, (<$>), pure)
import Data.Argonaut.Decode (class DecodeJson)
import Data.Argonaut.Encode (class EncodeJson, encodeJson)
import Data.Maybe (Maybe(..))
import PscIde.Command(
  Completion, Filter, Message, ModuleList, ImportList, Result,
  Command(..), Matcher(..), PursuitCompletion, unwrapResponse, ListType(..),
  PursuitType(..))

foreign import data NET :: !

foreign import send :: forall eff.
  String -> -- ^ Command
  Int ->  -- ^ Port
  (String -> Eff (net :: NET | eff) Unit) -> -- ^ Callback
  (Error -> Eff (net :: NET | eff) Unit) -> -- ^ Error Callback
  Eff (net :: NET | eff) Unit

type Cmd a = forall eff. Aff (net :: NET | eff) (Result a)

sendCommand :: forall i o. (EncodeJson i, DecodeJson o) => i -> Cmd o
sendCommand c = makeAff (\err succ -> send (show (encodeJson c)) 4242 (unwrapResponse >>> succ) err)

cwd :: Cmd Message
cwd = sendCommand Cwd


listLoadedModules :: Cmd ModuleList
listLoadedModules = sendCommand (Ls LoadedModules)


listAvailableModules :: Cmd ModuleList
listAvailableModules = sendCommand (Ls AvailableModules)


listImports :: String -> Cmd ImportList
listImports fp = sendCommand (Ls (Imports fp))


load ::
  Array String ->
  Array String ->
  Cmd Message
load ms ds = sendCommand (Load ms ds)


quit :: Cmd Message
quit = sendCommand Quit


pursuitCompletion :: String -> Cmd (Array PursuitCompletion)
pursuitCompletion q = sendCommand (Pursuit Ident q)


complete ::
  Array Filter ->
  Maybe Matcher ->
  Cmd (Array Completion)
complete fs m = sendCommand (Complete fs m)


suggestTypos ::
  String ->
  Int ->
  Cmd (Array Completion)
suggestTypos q m = (<|> (pure [])) <$> complete [] (Just (Distance q m))
