module PscIde.Project where

import Prelude
import Node.Path as NP
import Control.Monad.Eff (Eff)
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), contains)
import Node.FS (FS) as FS
import Node.FS.Sync (exists) as FS

-- | Get PureScript project root given a .purs file (identified by presence of bower.json)
getRoot :: forall eff'. NP.FilePath -> Eff (fs :: FS.FS | eff') (Maybe NP.FilePath)
getRoot path =
  let parent = getParent path
      bower = NP.concat [path, "bower.json"] in
  if path == "" || path == parent then
    pure Nothing
  else if contains (Pattern "bower_components") path then
    getRoot parent
  else do
    hasBower <- FS.exists bower
    if hasBower then pure $ Just path else getRoot parent

  where
    getParent :: NP.FilePath -> NP.FilePath
    getParent p = NP.concat [p, ".."]
