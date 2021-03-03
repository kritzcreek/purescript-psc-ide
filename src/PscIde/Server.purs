module PscIde.Server where

import Prelude

import Control.Alt ((<|>))
import Control.Parallel.Class (parallel, sequential)
import Data.Either (Either(..), either, hush)
import Data.Int as Int
import Data.Maybe (Maybe(..), maybe)
import Data.Time.Duration (Milliseconds(..))
import Data.Traversable (for)
import Effect (Effect)
import Effect.Aff (Aff, attempt, delay, makeAff, nonCanceler, try)
import Effect.Class (liftEffect)
import Effect.Random (randomInt)
import Foreign.Object (Object)
import Node.Buffer as Buffer
import Node.ChildProcess (ChildProcess, StdIOBehaviour, Exit(..), onClose, onError, defaultSpawnOptions, spawn, defaultExecOptions, execFile, pipe)
import Node.Encoding (Encoding(UTF8))
import Node.FS.Sync (readTextFile, unlink, writeTextFile)
import Node.Path as Path
import Node.Which (which')
import PscIde (quit)

data ServerStartResult =
  Started ChildProcess
  | Closed
  | StartError String

type PscIdeServerArgs = {
  exe :: String,
  combinedExe :: Boolean,
  cwd :: Maybe String,
  stdio :: Array (Maybe StdIOBehaviour),
  source :: Array String, -- source globs
  port :: Maybe Int,
  directory :: Maybe String,
  outputDirectory :: Maybe String,
  watch :: Boolean,
  debug :: Boolean,
  polling :: Boolean,
  editorMode :: Boolean,
  logLevel :: Maybe LogLevel
}

data LogLevel = All | None | Debug | Perf
logParam :: LogLevel -> String
logParam = case _ of
  All -> "all"
  None -> "none"
  Debug -> "debug"
  Perf -> "perf"

defaultServerArgs :: PscIdeServerArgs
defaultServerArgs = {
  exe: "purs",
  -- TODO: Remove combinedExe when support for the non-combined executable can be removed
  combinedExe: true,
  cwd: Nothing,
  stdio: pipe,
  source: [],
  port: Nothing,
  directory: Nothing,
  outputDirectory: Nothing,
  watch: true,
  debug: false,
  polling: false,
  editorMode: false,
  logLevel: Nothing
}

-- | Start a psc-ide server instance
startServer ∷ PscIdeServerArgs → Aff ServerStartResult
startServer { stdio, exe, combinedExe, cwd, source, port, directory, outputDirectory, watch, debug, polling, editorMode, logLevel } = do
    cp <- liftEffect (spawn exe (
      (if combinedExe then ["ide", "server"] else []) <>
      (maybe [] (\p -> ["-p", show p]) port) <>
      (maybe [] (\d -> ["-d", d]) directory) <>
      (maybe [] (\od -> ["--output-directory", od]) outputDirectory) <>
      (if watch then [] else ["--no-watch"]) <>
      (if debug then ["--debug"] else []) <>
      (if polling then ["--polling"] else []) <>
      (if editorMode then ["--editor-mode"] else []) <>
      (maybe [] (\l -> ["--log-level", logParam l]) logLevel) <>
      source
      ) defaultSpawnOptions { cwd = cwd, stdio = stdio })
    let handleErr = makeAff \ cb -> nonCanceler <$ do
                      onError cp (\{ code, errno, syscall } ->
                        cb $ Right $ StartError $
                          "psc-ide-server error:" <>
                          "{ code: " <> code <>
                          ", errno: " <> errno <>
                          ", syscall: " <> syscall <>
                          " }")
                      onClose cp (\exit -> case exit of
                                     (Normally 0) -> cb $ Right Closed
                                     (Normally n) -> cb $ Right $ StartError $ "Error code returned: "<> show n
                                     _ -> cb $ Right $ StartError "Other close error")

    sequential (parallel handleErr <|> parallel (delay (Milliseconds 100.0) $> Started cp))

-- | Construct path to the port file identifying the psc-ide-server port
portFilePath :: String -> String
portFilePath cwd = Path.concat [ cwd, ".psc-ide-port" ]

-- | Save a port to the port file
savePort :: Int → String → Effect Unit
savePort port cwd = writeTextFile UTF8 (portFilePath cwd) (show port)

-- | Delete the port file
deleteSavedPort :: String → Effect Unit
deleteSavedPort cwd = unlink (portFilePath cwd)

-- | Get the saved port for the given project directory (if present)
getSavedPort :: String → Effect (Maybe Int)
getSavedPort cwd = ado
  text <- try (readTextFile UTF8 (portFilePath cwd))
  in Int.fromString =<< hush text

-- | Generate a fresh port (just now, randomly with no check or retry)
pickFreshPort :: Effect Int
pickFreshPort = randomInt 15000 16000

-- | Stop a psc-ide server.
stopServer :: Int -> Aff Unit
stopServer port = void $ quit port

data Executable = Executable String (Maybe String)

findBins :: String -> Aff (Array Executable)
findBins = findBins' { path: Nothing, pathExt: Nothing, env: Nothing }

findBins' :: { path :: Maybe String, pathExt :: Maybe String, env :: Maybe (Object String) } -> String -> Aff (Array Executable)
findBins' { path, pathExt, env } executable = do
  bins <- which' { path, pathExt } executable <|> pure []
  for bins \bin -> Executable bin <$> either (const Nothing) Just <$> attempt (getVersion bin)

  where
  getVersion :: String -> Aff String
  getVersion bin = makeAff $ \cb -> nonCanceler <$
    execFile bin ["--version"] (defaultExecOptions { env = env }) \({error, stdout}) -> do
      maybe (Right <$> Buffer.readString UTF8 0 100 stdout >>= cb) (cb <<< Left) error
