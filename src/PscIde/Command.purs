module PscIde.Command where

import Control.Alt ((<|>))
import Data.Argonaut.Combinators ((~>), (:=), (.?))
import Data.Argonaut.Core (jsonEmptyObject, jsonSingletonObject, Json)
import Data.Argonaut.Decode (class DecodeJson, decodeJson)
import Data.Argonaut.Encode (class EncodeJson, encodeJson)
import Data.Argonaut.Parser (jsonParser)
import Data.Argonaut.Printer (printJson)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Prelude (class Show, show, pure, (<<<), bind, (<$>), ($))

data PursuitType = Package | Ident

instance showPursuitType :: Show PursuitType where
  show Package = "package"
  show Ident   = "completion"

instance encodePursuitType :: EncodeJson PursuitType where
  encodeJson = encodeJson <<< show

data Matcher =
  Flex String
  | Distance String Int


instance encodeMatcher :: EncodeJson Matcher where
  encodeJson (Flex q) =
    "matcher" := "flex"
    ~> "params" := (jsonSingletonObject' "search" q)
    ~> jsonEmptyObject
  encodeJson (Distance q maxDist) =
    "matcher" := "distance"
    ~> "params" := (
       "search" := q
       ~> "maximumDistance" := maxDist
       ~> jsonEmptyObject
      )
    ~> jsonEmptyObject

data Filter =
  ExactFilter String
  | PrefixFilter String
  | ModuleFilter (Array String)
  | DependencyFilter (Array String)

filterWrapper :: forall a. (EncodeJson a) => String -> a -> Json
filterWrapper f q =
  "filter" := f
  ~> ("params" := q)
  ~> jsonEmptyObject

jsonSingletonObject' :: forall a. (EncodeJson a) => String -> a -> Json
jsonSingletonObject' s o = jsonSingletonObject s (encodeJson o)

instance encodeFilter :: EncodeJson Filter where
  encodeJson (ExactFilter q) =
    filterWrapper "exact" (jsonSingletonObject' "search" q)
  encodeJson (PrefixFilter q) =
    filterWrapper "prefix" (jsonSingletonObject' "search" q)
  encodeJson (ModuleFilter modules) =
    filterWrapper "modules" (jsonSingletonObject' "modules" modules)
  encodeJson (DependencyFilter deps) =
    filterWrapper "dependencies" (jsonSingletonObject' "modules" deps)

data Command =
  Cwd
  | Ls ListType
  | Quit
  | Load (Array String) (Array String)
  | Complete (Array Filter) (Maybe Matcher)
  | Pursuit PursuitType String
  | Type String (Array Filter)

data ListType = LoadedModules | Imports String | AvailableModules

commandWrapper :: forall a. (EncodeJson a) => String -> a -> Json
commandWrapper s o =
  "command" := s
  ~> ("params" := o)
  ~> jsonEmptyObject

instance encodeCommand :: EncodeJson Command where
  encodeJson Cwd = jsonSingletonObject' "command" "cwd"
  encodeJson (Ls LoadedModules) =
    commandWrapper "list" ("type" := "loadedModules" ~> jsonEmptyObject)
  encodeJson (Ls AvailableModules) =
    commandWrapper "list" ("type" := "availableModules" ~> jsonEmptyObject)
  encodeJson (Ls (Imports fp)) =
    commandWrapper "list" ("type" := "import" ~> "file" := fp ~> jsonEmptyObject)
  encodeJson Quit = jsonSingletonObject' "command" "quit"
  encodeJson (Load modules dependencies) =
    commandWrapper "load" (
      "modules" := (encodeJson modules)
      ~> "dependencies" := (encodeJson dependencies)
      ~> jsonEmptyObject
      )
  encodeJson (Complete filters matcher) =
    commandWrapper "complete" (
      "filters" := (encodeJson filters)
      ~> "matcher" := (encodeJson matcher)
      ~> jsonEmptyObject
      )
  encodeJson (Pursuit psType q) =
    commandWrapper "pursuit" (
        "type" := (encodeJson psType)
        ~> "query" := q
        ~> jsonEmptyObject
      )
  encodeJson (Type text filters) =
    commandWrapper "type" (
        "search" := encodeJson text
        ~> "filters" := (encodeJson filters)
        ~> jsonEmptyObject
      )

type Result a = Either String a

type GenCompletion a = {
  type' :: String,
  identifier :: String,
  module' :: String
  | a
}

newtype Completion = Completion (GenCompletion ())
newtype PursuitCompletion = PursuitCompletion (GenCompletion (package :: String))
newtype ModuleList = ModuleList (Array String)
newtype Message = Message String
newtype ImportList = ImportList (Array Import)
newtype Import = Import
  {
    moduleName :: String,
    importType :: ImportType,
    qualifier  :: Maybe String
  }

data ImportType = Implicit | Explicit (Array String) | Hiding (Array String)

unwrapResponse :: forall a. (DecodeJson a) => String -> Result a
unwrapResponse s = do
  json <- jsonParser s
  o <- decodeJson json
  resultType <- o .? "resultType"
  case resultType of
    "success" -> do
      result <- o .? "result"
      Right result
    _ -> do
      result <- o .? "result"
      Left result

instance decodeMessage :: DecodeJson Message where
  decodeJson json = pure (Message (printJson json))

instance decodeModuleList :: DecodeJson ModuleList where
  decodeJson json = ModuleList <$> decodeJson json

instance decodeCompletion :: DecodeJson Completion where
  decodeJson json = do
    o <- decodeJson json
    identifier <- o .? "identifier"
    type' <- o .? "type"
    module' <- o .? "module"
    pure (Completion {identifier: identifier, type': type', module': module'})

instance decodePursuitCompletion :: DecodeJson PursuitCompletion where
  decodeJson json = do
    o <- decodeJson json
    identifier <- o .? "ident"
    type' <- o .? "type"
    module' <- o .? "module"
    package <- o .? "package"
    pure (PursuitCompletion {
      identifier: identifier,
      type': type',
      module': module',
      package: package
      })

instance decodeImportList :: DecodeJson ImportList where
  decodeJson json = do
    imports <- decodeJson json
    pure (ImportList imports)

instance decodeImport :: DecodeJson Import where
  decodeJson json = do
    o <- decodeJson json
    moduleName <- o .? "module"
    importType <- o .? "importType"
    case importType of
      "implicit" -> do
        q <- (Just <$> o .? "qualifier") <|> pure Nothing
        pure $ Import {moduleName: moduleName, importType: Implicit, qualifier: q}
      "explicit" -> do
        identifiers <- o .? "identifiers"
        pure $ Import {moduleName: moduleName, importType: Explicit identifiers, qualifier: Nothing}
      "hiding"   -> do
        identifiers <- o .? "identifiers"
        pure $ Import {moduleName: moduleName, importType: Hiding identifiers, qualifier: Nothing}
      _ -> Left "unknown importType"
