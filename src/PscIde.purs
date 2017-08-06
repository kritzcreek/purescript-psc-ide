module PscIde where

import PscIde.Command
import Control.Alt ((<|>))
import Control.Bind (join)
import Control.Monad.Aff (Aff, makeAff)
import Control.Monad.Eff (Eff, kind Effect)
import Control.Monad.Eff.Exception (Error)
import Data.Argonaut (class DecodeJson, class EncodeJson, encodeJson)
import Data.Either (Either)
import Data.Maybe (Maybe(..))
import Prelude (Unit, pure, (<$>), (>>>), show)

foreign import data NET :: Effect

foreign import send
  :: forall eff.
  String  -- ^ Command
  -> Int -- ^ Port
  -> (String -> Eff (net :: NET | eff) Unit)  -- ^ Callback
  -> (Error -> Eff (net :: NET | eff) Unit)  -- ^ Error Callback
  -> Eff (net :: NET | eff) Unit

type Cmd a = forall eff. Aff (net :: NET | eff) (Result a)
type CmdR a b = forall eff. Aff (net :: NET | eff) (Result (Either a b))

sendCommandR :: forall i oe o. EncodeJson i => DecodeJson oe => DecodeJson o => Int -> i -> CmdR oe o
sendCommandR port command =
  makeAff \err succ ->
            send (show (encodeJson command)) port (unwrapResponse >>> succ) err

sendCommand :: forall i o. EncodeJson i => DecodeJson o => Int -> i -> Cmd o
sendCommand port command =
  makeAff \err succ ->
            send (show (encodeJson command)) port (unwrapResponse >>> join >>> succ) err

cwd :: Int -> Cmd Message
cwd port = sendCommand port Cwd

listLoadedModules :: Int -> Cmd ModuleList
listLoadedModules port = sendCommand port (Ls LoadedModules)

listAvailableModules :: Int -> Cmd ModuleList
listAvailableModules port = sendCommand port (Ls AvailableModules)

listImports :: Int -> String -> Cmd ImportList
listImports port fp = sendCommand port (Ls (Imports fp))

load :: Int -> Array String -> Array String -> Cmd Message
load port ms ds = sendCommand port (Load ms ds)

quit :: Int -> Cmd Message
quit port = sendCommand port Quit

reset :: Int -> Cmd Message
reset port = sendCommand port Reset

pursuitCompletion :: Int -> String -> Cmd (Array PursuitCompletion)
pursuitCompletion port q = sendCommand port (Pursuit Ident q)

defaultCompletionOptions :: CompletionOptions
defaultCompletionOptions = CompletionOptions {
  maxResults: Nothing,
  groupReexports: false
}

complete :: Int -> Array Filter -> Maybe Matcher -> Maybe String -> CompletionOptions -> Cmd (Array TypeInfo)
complete port fs m mod opts = sendCommand port (Complete fs m mod opts)

type':: Int -> String -> Array Filter -> Maybe String-> Cmd (Array TypeInfo)
type' port s fs mod = sendCommand port (Type s fs mod)

suggestTypos :: Int -> String -> Int -> Maybe String -> CompletionOptions-> Cmd (Array TypeInfo)
suggestTypos port q m mod opts = (_ <|> pure []) <$> complete port [] (Just (Distance q m)) mod opts

addClause :: Int -> String -> Boolean -> Cmd (Array String)
addClause port line annotations = sendCommand port (AddClause line annotations)

caseSplit :: Int -> String -> Int -> Int -> Boolean -> String -> Cmd (Array String)
caseSplit port line begin end annotations typ =
  sendCommand port (CaseSplit line begin end annotations typ)

implicitImport :: Int -> String -> (Maybe String) -> (Array Filter) -> String -> Cmd (ImportResult)
implicitImport port infile outfile filters mod = sendCommand port (ImportCmd infile outfile filters (AddImplicitImport mod))

explicitImport :: Int -> String -> (Maybe String) -> (Array Filter) -> String -> Maybe String -> Cmd (ImportResult)
explicitImport port infile outfile filters ident qualifier = sendCommand port (ImportCmd infile outfile filters (AddImport ident qualifier))

rebuild :: Int -> String -> CmdR RebuildResult RebuildResult
rebuild port file = sendCommandR port (RebuildCmd file)
