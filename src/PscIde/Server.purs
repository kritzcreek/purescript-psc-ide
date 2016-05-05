module PscIde.Server where

import Prelude
import Node.Buffer as Buffer
import Data.Either (either)
import Data.Maybe (Maybe(Just, Nothing), maybe)
import Data.Traversable (for)
import Node.Buffer (BUFFER)
import Node.Encoding (Encoding(UTF8))
import Node.FS (FS)
import Node.Which (which)
import Control.Alt ((<|>))
import Control.Monad.Aff (attempt, Aff, later', makeAff)
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Aff.Par (Par(Par), runPar)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE)
import Node.ChildProcess (CHILD_PROCESS, ChildProcess, Exit(Normally), onClose, onError, defaultSpawnOptions, spawn, defaultExecOptions, execFile)
import PscIde (NET, quit)

data ServerStartResult =
  Started ChildProcess
  | Closed
  | StartError String

-- | Start a psc-ide server instance
startServer ∷ forall eff. String → Int -> Maybe String
  → Aff (cp ∷ CHILD_PROCESS, console ∷ CONSOLE, avar ∷ AVAR | eff) ServerStartResult
startServer exe port projectRoot = do
    cp <- liftEff (spawn exe ["-p", show port] defaultSpawnOptions { cwd = projectRoot })
    let handleErr = makeAff \_ succ -> do
                      onError cp (\_ -> succ $ StartError "psc-ide-server error")
                      onClose cp (\exit -> case exit of
                                     (Normally 0) -> succ Closed
                                     (Normally n) -> succ $ StartError $ "Error code returned: "++ show n
                                     _ -> succ $ StartError "Other close error")

    runPar (Par handleErr <|> Par (later' 100 $ pure $ Started cp))

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
