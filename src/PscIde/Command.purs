module PscIde.Command where

import Prelude

import Control.Alt ((<|>))
import Data.Argonaut (JsonDecodeError(..), getField, parseJson, printJsonDecodeError)
import Data.Argonaut.Core (jsonEmptyObject, jsonSingletonObject, jsonNull, fromString, Json, toString)
import Data.Argonaut.Decode (class DecodeJson, decodeJson, (.:))
import Data.Argonaut.Encode (class EncodeJson, encodeJson, (~>), (:=))
import Data.Array (singleton)
import Data.Bifunctor (lmap)
import Data.Either (Either(..), either, hush)
import Data.Maybe (Maybe(..), maybe)
import Data.String (joinWith)
import Foreign.Object (Object)

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
  | NamespaceFilter (Array Namespace)
  | DeclarationFilter (Array DeclarationType)

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
  encodeJson (NamespaceFilter nss) =
    filterWrapper "namespace" (jsonSingletonObject' "namespaces" nss)
  encodeJson (DeclarationFilter decls) =
    filterWrapper "declarations" (map declarationTypeToString decls)

newtype CompletionOptions = CompletionOptions {
  maxResults :: Maybe Int,
  groupReexports :: Boolean
}

instance encodeCompletionOptions :: EncodeJson CompletionOptions where
  encodeJson (CompletionOptions { maxResults, groupReexports }) =
   "maxResults" := encodeMaybeNull maxResults
    ~> "groupReexports" := encodeJson groupReexports
    ~> jsonEmptyObject

data Command =
  Cwd
  | Ls ListType
  | Quit
  | Reset
  | Load (Array String) (Array String)
  | Complete (Array Filter) (Maybe Matcher) (Maybe String) CompletionOptions
  | Pursuit PursuitType String
  | Type String (Array Filter) (Maybe String)
  | AddClause String Boolean
  | CaseSplit String Int Int Boolean String
  | ImportCmd FileName (Maybe FileName) (Array Filter) ImportCommand
  | RebuildCmd String (Maybe FileName) (Maybe (Array CodegenTarget))
  | Usages String Namespace String

data ListType = LoadedModules | Imports String | AvailableModules
data Namespace = NSValue | NSType | NSKind 
data CodegenTarget =  JS | JSSourceMap | CoreFn | Other String

type FileName = String
data ImportCommand = AddImplicitImport String | AddQualifiedImport String String | AddImport String (Maybe String)

data DeclarationType = 
  DeclValue
  | DeclType
  | DeclTypeSynonym
  | DeclDataConstructor
  | DeclTypeClass
  | DeclValueOperator
  | DeclTypeOperator
  | DeclModule

declarationTypeFromString :: String -> Maybe DeclarationType
declarationTypeFromString = case _ of 
  "value" -> Just DeclValue
  "type" -> Just DeclType
  "synonym" -> Just DeclTypeSynonym
  "dataconstructor" -> Just DeclDataConstructor
  "typeclass" -> Just DeclTypeClass
  "valueoperator" -> Just DeclValueOperator
  "typeoperator" -> Just DeclTypeOperator
  "module" -> Just DeclModule
  _ -> Nothing

declarationTypeToString :: DeclarationType -> String
declarationTypeToString = case _ of 
  DeclValue -> "value" 
  DeclType -> "type" 
  DeclTypeSynonym -> "synonym" 
  DeclDataConstructor -> "dataconstructor" 
  DeclTypeClass -> "typeclass" 
  DeclValueOperator -> "valueoperator" 
  DeclTypeOperator -> "typeoperator" 
  DeclModule -> "module" 

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
  encodeJson Reset = jsonSingletonObject' "command" "reset"
  encodeJson (Load modules dependencies) =
    commandWrapper "load" (
      "modules" := (encodeJson modules)
      ~> "dependencies" := (encodeJson dependencies)
      ~> jsonEmptyObject
      )
  encodeJson (Complete filters matcher currentModule options) =
    commandWrapper "complete" (
      "filters" := (encodeJson filters)
      ~> "matcher" := (encodeMaybeNull matcher)
      ~> "currentModule" := (encodeMaybeNull currentModule)
      ~> "options" := (encodeJson options)
      ~> jsonEmptyObject
      )
  encodeJson (Pursuit psType q) =
    commandWrapper "pursuit" (
        "type" := (encodeJson psType)
        ~> "query" := q
        ~> jsonEmptyObject
      )
  encodeJson (Type text filters currentModule) =
    commandWrapper "type" (
        "search" := encodeJson text
        ~> "filters" := (encodeJson filters)
        ~> "currentModule" := (encodeMaybeNull currentModule)
        ~> jsonEmptyObject
      )
  encodeJson (AddClause line annotations) =
    commandWrapper "addClause" (
        "line" := encodeJson line
        ~> "annotations" := encodeJson annotations
        ~> jsonEmptyObject
      )
  encodeJson (CaseSplit line begin end annotations typ) =
    commandWrapper "caseSplit" (
        "line" := encodeJson line
        ~> "begin" := encodeJson begin
        ~> "end" := encodeJson end
        ~> "annotations" := encodeJson annotations
        ~> "type" := encodeJson typ
        ~> jsonEmptyObject
      )
  encodeJson (ImportCmd inFile outFile filters cmd) =
    commandWrapper "import" (
      "file" := encodeJson inFile
      ~> "outfile" := encodeMaybeNull outFile
      ~> "filters" := encodeJson filters
      ~> "importCommand" := encodeJson cmd
      ~> jsonEmptyObject
      )
  encodeJson (RebuildCmd file actualFile codegen) =
    commandWrapper "rebuild" (
      "file" := encodeJson file
      ~> "actualFile" := encodeMaybeNull actualFile
      ~> "codegen" := encodeMaybeNull codegen
      ~> jsonEmptyObject
      )
  encodeJson (Usages mod ns ident) = 
    commandWrapper "usages" (
      "module" := encodeJson mod
      ~> "namespace" := encodeJson ns
      ~> "identifier" := encodeJson ident
      ~> jsonEmptyObject
    )

encodeMaybeNull :: forall a. (EncodeJson a) => Maybe a -> Json
encodeMaybeNull = maybe jsonNull encodeJson

instance encodeImportCommand :: EncodeJson ImportCommand where
  encodeJson (AddImplicitImport ident) =
    "importCommand" := "addImplicitImport"
    ~> "module" := encodeJson ident
    ~> jsonEmptyObject
  encodeJson (AddQualifiedImport mod qualifier) =
    "importCommand" := "addQualifiedImport"
    ~> "module" := encodeJson mod
    ~> "qualifier" := encodeJson qualifier
    ~> jsonEmptyObject
  encodeJson (AddImport mod qualifier) =
    "importCommand" := "addImport"
    ~> "identifier" := encodeJson mod
    ~> "qualifier" := encodeJson qualifier
    ~> jsonEmptyObject

instance encodeNamespace :: EncodeJson Namespace where
  encodeJson NSValue = fromString "value"
  encodeJson NSType = fromString "type"
  encodeJson NSKind = fromString "kind"

instance encodeCodegenTarget :: EncodeJson CodegenTarget where
  encodeJson JS = fromString "js"
  encodeJson JSSourceMap = fromString "sourcemaps"
  encodeJson CoreFn = fromString "corefn"
  encodeJson (Other s) = fromString s

type Result a = Either String a

type GenCompletion a = {
  type' :: String,
  identifier :: String,
  module' :: String
  | a
}

newtype TypePosition = TypePosition {
  name :: String,
  start :: Position,
  end :: Position
}

newtype TypeInfo = TypeInfo (GenCompletion (
  definedAt :: Maybe TypePosition,
  expandedType :: Maybe String,
  documentation :: Maybe String,
  exportedFrom :: Array String,
  declarationType :: Maybe DeclarationType
))

newtype PursuitCompletion = PursuitCompletion {
  type' :: Maybe String,
  identifier :: String,
  module' :: String,
  package :: String,
  text :: String
  }

newtype ModuleList = ModuleList (Array String)
newtype Message = Message String
newtype ImportList = ImportList { moduleName :: Maybe String, imports :: Array Import }
newtype Import = Import
  {
    moduleName :: String,
    importType :: ImportType,
    qualifier  :: Maybe String
  }

type Position = { line :: Int, column :: Int }
type RangePosition = { startLine :: Int, startColumn :: Int, endLine :: Int, endColumn :: Int }

newtype RebuildError =
  RebuildError
  { position :: Maybe RangePosition
  , moduleName :: Maybe String
  , filename :: Maybe String
  , errorCode :: String
  , message :: String
  , errorLink :: String
  , pursIde :: Maybe PursIdeInfo
  , suggestion :: Maybe PscSuggestion
  }

newtype PscSuggestion =
  PscSuggestion
  { replacement :: String
  , replaceRange :: Maybe RangePosition
  }

newtype RebuildResult = RebuildResult (Array RebuildError)

newtype PursIdeInfo =
  PursIdeInfo
  { name :: String
  , completions :: Array TypeInfo
  }

data ImportType = Implicit | Explicit (Array String) | Hiding (Array String)

data ImportResult = SuccessFile Message | SuccessText (Array String) | MultipleResults (Array TypeInfo)

unwrapResponse :: forall a b. DecodeJson a => DecodeJson b => String -> Either String (Either a b)
unwrapResponse s = lmap printJsonDecodeError do
  json <- parseJson s
  o <- decodeJson json
  resultType <- o .: "resultType"
  case resultType of
    "success" -> do
      result <- o .: "result"
      pure (Right result)
    _ -> do
      result <- o .: "result"
      pure (Left result)

instance decodeMessage :: DecodeJson Message where
  decodeJson json = maybe (Left (TypeMismatch "String")) (Right <<< Message) $ toString json

instance decodeModuleList :: DecodeJson ModuleList where
  decodeJson json = ModuleList <$> decodeJson json

instance decodeTypeInfo :: DecodeJson TypeInfo where
  decodeJson json = do
    o <- decodeJson json
    identifier <- o .: "identifier"
    type' <- o .: "type"
    module' <- o .: "module"
    definedAt <- o `getFieldMaybe` "definedAt"
    expandedType <- o `getFieldMaybe` "expandedType"
    documentation <- o `getFieldMaybe` "documentation"
    declarationTypeStr <- o `getFieldMaybe` "declarationType"
    -- TODO: Handling both missing/incorrect exportedFrom. Remove this after 0.12
    exportedFrom <- Right $ either (const []) identity $ getField o "exportedFrom"
    pure (TypeInfo 
      { identifier
      , type'
      , module'
      , definedAt
      , expandedType
      , documentation
      , exportedFrom
      , declarationType: declarationTypeFromString =<< declarationTypeStr
      })
    where
    getFieldMaybe :: forall a. (DecodeJson a) => Object Json -> String -> Either JsonDecodeError (Maybe a)
    getFieldMaybe o f = Right $ either (const Nothing) Just $ getField o f

instance decodeTypePosition :: DecodeJson TypePosition where
  decodeJson json = do
    o <- decodeJson json
    name <- o .: "name"
    start <- o .: "start"
    end <- o .: "end"
    case start, end of
      [sl, sc], [el, ec] ->
        Right (TypePosition { name, start: { line: sl, column: sc }, end: { line: el, column: ec } })
      _, _ -> Left (TypeMismatch "Array")

instance decodePursuitCompletion :: DecodeJson PursuitCompletion where
  decodeJson json = do
    o <- decodeJson json
    identifier <- o .: "ident"
    type' <- o .: "type"
    module' <- o .: "module"
    package <- o .: "package"
    text <- o .: "text"
    pure (PursuitCompletion {
      identifier: identifier,
      type': type',
      module': module',
      package: package,
      text: text
      })

instance decodeImportList :: DecodeJson ImportList where
  decodeJson json = decodeObject <|> decodeArray
    where
      decodeObject = do
        o <- decodeJson json
        moduleNameField <- o .: "moduleName"
        moduleName <- decodeModuleNameBug moduleNameField <|> decodeJson moduleNameField
        imports <- o .: "imports"
        pure (ImportList { moduleName: Just moduleName, imports })
      -- TODO: Working around a bug where module Foo.Bar is jsonified as ["Foo", "Bar"] instead of "Foo.Bar"
      decodeModuleNameBug moduleName = joinWith "." <$> decodeJson moduleName
      decodeArray = do
        imports <- decodeJson json
        pure (ImportList { moduleName: Nothing, imports })

instance decodeImport :: DecodeJson Import where
  decodeJson json = do
    o <- decodeJson json
    moduleName <- o .: "module"
    importType <- o .: "importType"
    q <- (Just <$> o .: "qualifier") <|> pure Nothing
    case importType of
      "implicit" -> do
        pure $ Import {moduleName: moduleName, importType: Implicit, qualifier: q}
      "explicit" -> do
        identifiers <- o .: "identifiers"
        pure $ Import {moduleName: moduleName, importType: Explicit identifiers, qualifier: q}
      "hiding"   -> do
        identifiers <- o .: "identifiers"
        pure $ Import {moduleName: moduleName, importType: Hiding identifiers, qualifier: q}
      _ -> Left (TypeMismatch "ImportType")

instance decodeImportResult :: DecodeJson ImportResult where
  decodeJson json = do
    (SuccessText <$> decodeJson json)
    <|> (SuccessFile <$> decodeJson json)
    <|> (MultipleResults <$> decodeJson json)
    <|> (Left (TypeMismatch "ImportResult"))

instance decodeRebuildError :: DecodeJson RebuildError where
  decodeJson json = do
    o <- decodeJson json
    message <- o .: "message"
    errorCode <- o .: "errorCode"
    errorLink <- o .: "errorLink"
    moduleName <- o .: "moduleName"
    filename <- o .: "filename"
    position <- pure $ hush do
      p <- o .: "position"
      { startLine: _, startColumn: _, endLine: _, endColumn: _ } <$> p .: "startLine" <*> p .: "startColumn" <*> p .: "endLine" <*> p .: "endColumn"
    pursIde <- pure $ hush do
      pio <- o .: "pursIde"
      name <- pio .: "name"
      completions <- pio .: "completions"
      pure $ PursIdeInfo { name, completions }
    suggestion <- pure $ hush do
      so <- o .: "suggestion"
      replacement <- so .: "replacement"
      rr <- so .: "replaceRange"
      replaceRange <- pure $ hush $ { startLine: _, startColumn: _, endLine: _, endColumn: _ } <$> rr .: "startLine" <*> rr .: "startColumn" <*> rr .: "endLine" <*> rr .: "endColumn"
      pure $ PscSuggestion { replacement, replaceRange }
    pure (RebuildError { errorCode, errorLink, moduleName, filename, message, position, pursIde, suggestion })

instance decodeRebuildResult :: DecodeJson RebuildResult where
  decodeJson json = RebuildResult <$> (decodeJson json <|> (singleton <$> decodeJson json))
