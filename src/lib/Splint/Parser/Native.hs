module Splint.Parser.Native
  ( parse
  )
where

import qualified GhcPlugins as GHC
import qualified Language.Haskell.HLint as HLint

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
