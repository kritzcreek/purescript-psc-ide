module PscIde.Project where

import Prelude

import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), contains)
import Effect (Effect)
import Node.FS.Sync (exists) as FS
import Node.Path as NP

-- | Get PureScript project root given a .purs file (identified by presence of bower.json)
getRoot :: NP.FilePath -> Effect (Maybe NP.FilePath)
getRoot path =
  let parent = getParent path
      bower = NP.concat [path, "bower.json"]
      pscPackage = NP.concat [path, "psc-package.json"] in
  if path == "" || path == parent then
    pure Nothing
  else if contains (Pattern "bower_components") path || contains (Pattern ".psc-package") path then
    getRoot parent
  else do
    hasBower <- FS.exists bower
    hasPscPackage <- FS.exists pscPackage
    if hasBower || hasPscPackage then pure $ Just path else getRoot parent

  where
    getParent :: NP.FilePath -> NP.FilePath
    getParent p = NP.concat [p, ".."]
