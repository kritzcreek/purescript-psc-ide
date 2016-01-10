## Module PscIde

#### `NET`

``` purescript
data NET :: !
```

#### `send`

``` purescript
send :: forall eff. String -> Int -> (String -> Eff eff Unit) -> Eff (net :: NET | eff) Unit
```

#### `Cmd`

``` purescript
type Cmd a = forall eff. (Result a -> Eff eff Unit) -> Eff (net :: NET | eff) Unit
```

#### `sendCommand`

``` purescript
sendCommand :: forall i o eff. (EncodeJson i, DecodeJson o) => i -> (Result o -> Eff eff Unit) -> Eff (net :: NET | eff) Unit
```

#### `cwd`

``` purescript
cwd :: Cmd Message
```

#### `listLoadedModules`

``` purescript
listLoadedModules :: Cmd ModuleList
```

#### `listAvailableModules`

``` purescript
listAvailableModules :: Cmd ModuleList
```

#### `listImports`

``` purescript
listImports :: String -> Cmd ImportList
```

#### `load`

``` purescript
load :: Array String -> Array String -> Cmd Message
```

#### `quit`

``` purescript
quit :: Cmd Message
```

#### `pursuitCompletion`

``` purescript
pursuitCompletion :: String -> Cmd (Array PursuitCompletion)
```

#### `complete`

``` purescript
complete :: Array Filter -> Maybe Matcher -> Cmd (Array Completion)
```

#### `suggestTypos`

``` purescript
suggestTypos :: forall eff. String -> Int -> (Array Completion -> Eff eff Unit) -> Eff (net :: NET | eff) Unit
```


