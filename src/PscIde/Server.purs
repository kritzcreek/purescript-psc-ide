module PscIde.Server where

import Prelude
import Node.Buffer as Buffer
import Node.Path as Path
import Control.Alt ((<|>))
import Control.Monad.Aff (attempt, Aff, later', makeAff)
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Parallel.Class (parallel, runParallel)
import Node.ChildProcess (CHILD_PROCESS, ChildProcess, StdIOBehaviour,
                         Exit(Normally), onClose, onError, defaultSpawnOptions,
                         spawn, defaultExecOptions, execFile, pipe)
import Control.Monad.Eff.Exception (EXCEPTION, catchException)
import Control.Monad.Eff.Random (RANDOM, randomInt)
import Data.Either (either)
import Data.Int (fromNumber)
import Data.Maybe (Maybe(Just, Nothing), maybe)
import Data.Traversable (for)
import Global (readInt)
import Node.Buffer (BUFFER)
import Node.Encoding (Encoding(UTF8))
import Node.FS (FS)
import Node.FS.Sync (readTextFile, unlink, writeTextFile)
import Node.Which (which)
import PscIde (NET, quit)

data ServerStartResult =
  Started ChildProcess
  | Closed
  | StartError String

type PscIdeServerArgs = {
  exe :: String,
  cwd :: Maybe String,
  stdio :: Array (Maybe StdIOBehaviour),
  source :: Array String, -- source globs
  port :: Maybe Int,
  directory :: Maybe String,
  outputDirectory :: Maybe String,
  watch :: Boolean,
  debug :: Boolean
}

defaultServerArgs :: PscIdeServerArgs
defaultServerArgs = {
  exe: "psc-ide-server",
  cwd: Nothing,
  stdio: pipe,
  source: [],
  port: Nothing,
  directory: Nothing,
  outputDirectory: Nothing,
  watch: true,
  debug: false
}

-- | Start a psc-ide server instance
startServer ∷ forall eff.  PscIdeServerArgs → Aff (cp ∷ CHILD_PROCESS, console ∷ CONSOLE, avar ∷ AVAR | eff) ServerStartResult
startServer { stdio, exe, cwd, source, port, directory, outputDirectory, watch, debug } = do
    cp <- liftEff (spawn exe (
      (maybe [] (\p -> ["-p", show p]) port) <>
      (maybe [] (\d -> ["-d", d]) directory) <>
      (maybe [] (\od -> ["--output-directory", od]) outputDirectory) <>
      (if watch then [] else ["--no-watch"]) <>
      (if debug then ["--debug"] else []) <>
      source
      ) defaultSpawnOptions { cwd = cwd, stdio = stdio })
    let handleErr = makeAff \_ succ -> do
                      onError cp (\_ -> succ $ StartError "psc-ide-server error")
                      onClose cp (\exit -> case exit of
                                     (Normally 0) -> succ Closed
                                     (Normally n) -> succ $ StartError $ "Error code returned: "<> show n
                                     _ -> succ $ StartError "Other close error")

    runParallel (parallel handleErr <|> parallel (later' 100 $ pure $ Started cp))

-- | Construct path to the port file identifying the psc-ide-server port
portFilePath :: String -> String
portFilePath cwd = Path.concat [ cwd, ".psc-ide-port" ]

-- | Save a port to the port file
savePort :: forall eff. Int → String → Eff (fs :: FS, err :: EXCEPTION | eff) Unit
savePort port cwd = writeTextFile UTF8 (portFilePath cwd) (show port)

-- | Delete the port file
deleteSavedPort :: forall eff. String → Eff (fs :: FS, err :: EXCEPTION | eff) Unit
deleteSavedPort cwd = unlink (portFilePath cwd)

-- | Get the saved port for the given project directory (if present)
getSavedPort :: forall eff. String → Eff (fs :: FS | eff) (Maybe Int)
getSavedPort cwd = do
  text <- catchException (\_ -> pure Nothing) (Just <$> readTextFile UTF8 (portFilePath cwd))
  pure $ maybe Nothing (fromNumber <<< readInt 10) text

-- | Generate a fresh port (just now, randomly with no check or retry)
pickFreshPort :: forall eff. Eff (random :: RANDOM | eff) Int
pickFreshPort = randomInt 15000 16000

-- | Stop a psc-ide server.
stopServer :: forall eff. Int -> Aff (cp :: CHILD_PROCESS, net :: NET | eff) Unit
stopServer port = void $ quit port

data Executable = Executable String (Maybe String)

findBins :: forall eff. String -> Aff (fs :: FS, buffer :: BUFFER, cp :: CHILD_PROCESS | eff) (Array Executable)
findBins exe = do
  bins <- which exe <|> pure []
  for bins \exe -> Executable exe <$> either (const Nothing) Just <$> attempt (getVersion exe)

  where
  getVersion :: forall eff'. String -> Aff (buffer :: BUFFER, cp :: CHILD_PROCESS | eff') String
  getVersion exe = makeAff $ \err succ ->
    execFile exe ["--version"] defaultExecOptions \({error, stdout}) -> do
      maybe (Buffer.readString UTF8 0 100 stdout >>= succ) err error
