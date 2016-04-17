module PscIde where

import PscIde.Command
import Control.Alt ((<|>))
import Control.Bind (join)
import Control.Monad.Aff (Aff, makeAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (Error)
import Data.Argonaut (class DecodeJson, class EncodeJson, encodeJson)
import Data.Either (Either)
import Data.Maybe (Maybe(..))
import Prelude (Unit, pure, (<$>), (>>>), show)

foreign import data NET :: !

foreign import send :: forall eff.
  String -> -- ^ Command
  Int ->  -- ^ Port
  (String -> Eff (net :: NET | eff) Unit) -> -- ^ Callback
  (Error -> Eff (net :: NET | eff) Unit) -> -- ^ Error Callback
  Eff (net :: NET | eff) Unit

type Cmd a = forall eff. Aff (net :: NET | eff) (Result a)
type CmdR a b = forall eff. Aff (net :: NET | eff) (Result (Either a b))

sendCommandR :: forall i oe o. (EncodeJson i, DecodeJson oe, DecodeJson o) => i -> CmdR oe o
sendCommandR c = makeAff (\err succ -> send (show (encodeJson c)) 4242 (unwrapResponse >>> succ) err)

sendCommand :: forall i o. (EncodeJson i, DecodeJson o) => i -> Cmd o
sendCommand c = makeAff (\err succ -> send (show (encodeJson c)) 4242 (unwrapResponse >>> join >>> succ) err)


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

type' ::
  String ->
  Array Filter ->
  Cmd (Array Completion)
type' s fs = sendCommand (Type s fs)

suggestTypos ::
  String ->
  Int ->
  Cmd (Array Completion)
suggestTypos q m = (_ <|> pure []) <$> complete [] (Just (Distance q m))

addClause :: String -> Boolean -> Cmd (Array String)
addClause line annotations = sendCommand (AddClause line annotations)

caseSplit :: String -> Int -> Int -> Boolean -> String -> Cmd (Array String)
caseSplit line begin end annotations typ =
  sendCommand (CaseSplit line begin end annotations typ)

implicitImport :: String -> (Maybe String) -> (Array Filter) -> String -> Cmd (ImportResult)
implicitImport infile outfile filters mod = sendCommand (ImportCmd infile outfile filters (AddImplicitImport mod))

explicitImport :: String -> (Maybe String) -> (Array Filter) -> String -> Cmd (ImportResult)
explicitImport infile outfile filters ident = sendCommand (ImportCmd infile outfile filters (AddImport ident))

rebuild :: String -> CmdR (Array RebuildError) (Array RebuildError)
rebuild file = sendCommandR (RebuildCmd file)
