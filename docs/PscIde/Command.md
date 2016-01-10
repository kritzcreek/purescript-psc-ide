## Module PscIde.Command

#### `PursuitType`

``` purescript
data PursuitType
  = Package
  | Ident
```

##### Instances
``` purescript
Show PursuitType
EncodeJson PursuitType
```

#### `Matcher`

``` purescript
data Matcher
  = Flex String
  | Distance String Int
```

##### Instances
``` purescript
EncodeJson Matcher
```

#### `Filter`

``` purescript
data Filter
  = ExactFilter String
  | PrefixFilter String
  | ModuleFilter (Array String)
  | DependencyFilter (Array String)
```

##### Instances
``` purescript
EncodeJson Filter
```

#### `filterWrapper`

``` purescript
filterWrapper :: forall a. (EncodeJson a) => String -> a -> Json
```

#### `jsonSingletonObject'`

``` purescript
jsonSingletonObject' :: forall a. (EncodeJson a) => String -> a -> Json
```

#### `Command`

``` purescript
data Command
  = Cwd
  | Ls ListType
  | Quit
  | Load (Array String) (Array String)
  | Complete (Array Filter) (Maybe Matcher)
  | Pursuit PursuitType String
```

##### Instances
``` purescript
EncodeJson Command
```

#### `ListType`

``` purescript
data ListType
  = LoadedModules
  | Imports String
  | AvailableModules
```

#### `commandWrapper`

``` purescript
commandWrapper :: forall a. (EncodeJson a) => String -> a -> Json
```

#### `Result`

``` purescript
type Result a = Either String a
```

#### `GenCompletion`

``` purescript
type GenCompletion a = { type' :: String, identifier :: String, module' :: String | a }
```

#### `Completion`

``` purescript
newtype Completion
  = Completion (GenCompletion ())
```

##### Instances
``` purescript
DecodeJson Completion
```

#### `PursuitCompletion`

``` purescript
newtype PursuitCompletion
  = PursuitCompletion (GenCompletion (package :: String))
```

##### Instances
``` purescript
DecodeJson PursuitCompletion
```

#### `ModuleList`

``` purescript
newtype ModuleList
  = ModuleList (Array String)
```

##### Instances
``` purescript
DecodeJson ModuleList
```

#### `Message`

``` purescript
newtype Message
  = Message String
```

##### Instances
``` purescript
DecodeJson Message
```

#### `ImportList`

``` purescript
newtype ImportList
  = ImportList (Array Import)
```

##### Instances
``` purescript
DecodeJson ImportList
```

#### `Import`

``` purescript
newtype Import
  = Import { moduleName :: String, importType :: ImportType, qualifier :: Maybe String }
```

##### Instances
``` purescript
DecodeJson Import
```

#### `ImportType`

``` purescript
data ImportType
  = Implicit
  | Explicit (Array String)
  | Hiding (Array String)
```

#### `unwrapResponse`

``` purescript
unwrapResponse :: forall a. (DecodeJson a) => String -> Result a
```


