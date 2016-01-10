module PscIde where

import Control.Monad.Eff (Eff)
import Prelude (Unit, (>>>), show)
import Data.Argonaut.Decode (class DecodeJson)
import Data.Argonaut.Encode (class EncodeJson, encodeJson)
import Data.Maybe (Maybe(..))
import Data.Either (Either(..))
import PscIde.Command(
  Completion, Filter, Message, ModuleList, ImportList, Result,
  Command(..), Matcher(..), PursuitCompletion, unwrapResponse, ListType(..),
  PursuitType(..))


foreign import data NET :: !


foreign import send :: forall eff.
  String -> -- ^ Command
  Int ->  -- ^ Port
  (String -> Eff eff Unit) -> -- ^ Callback
  Eff (net :: NET | eff) Unit


type Cmd a = forall eff.
  (Result a -> Eff eff Unit) ->
  Eff (net :: NET | eff) Unit


sendCommand :: forall i o eff. (EncodeJson i, DecodeJson o) =>
  i ->
  (Result o -> Eff eff Unit) ->
  Eff (net :: NET | eff) Unit
sendCommand c cb = send (show (encodeJson c)) 4242 (unwrapResponse >>> cb)


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


suggestTypos :: forall eff.
  String ->
  Int ->
  (Array Completion -> Eff eff Unit) ->
  Eff (net :: NET | eff) Unit
suggestTypos q m cb = complete [] (Just (Distance q m)) handle
  where
    handle (Right cs) = cb cs
    handle (Left err) = cb []
