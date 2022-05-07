module Splint.Parser where

import qualified Language.Haskell.HLint as HLint
import qualified GhcPlugins as GHC

parse
  :: HLint.ParseFlags
  -> GHC.ModSummary
  -> GHC.HsParsedModule
  -> GHC.Hsc HLint.ModuleEx
parse _ _ hsParsedModule = do
  let
    apiAnns = GHC.hpm_annotations hsParsedModule
    hsModule = GHC.hpm_module hsParsedModule
  pure $ HLint.createModuleEx apiAnns hsModule
