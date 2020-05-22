module Splint ( plugin ) where

import qualified Bag as GHC
import qualified ErrUtils as GHC
import qualified GhcPlugins as GHC
import qualified Language.Haskell.HLint as HLint

plugin :: GHC.Plugin
plugin = GHC.defaultPlugin
  { GHC.parsedResultAction = action
  , GHC.pluginRecompile = GHC.purePlugin
  }

action
  :: [GHC.CommandLineOption]
  -> GHC.ModSummary
  -> GHC.HsParsedModule
  -> GHC.Hsc GHC.HsParsedModule
action _ _ hsParsedModule = do
  dynFlags <- GHC.getDynFlags
  GHC.liftIO $ do
    (_parseFlags, classifies, hint) <- HLint.autoSettings
    let
      apiAnns = GHC.hpm_annotations hsParsedModule
      hsModule = GHC.hpm_module hsParsedModule
      moduleEx = HLint.createModuleEx apiAnns hsModule
      ideas = HLint.applyHints classifies hint [moduleEx]
    GHC.printOrThrowWarnings dynFlags
      . GHC.listToBag
      . fmap (ideaToWarnMsg dynFlags)
      $ filter ((/= HLint.Ignore) . HLint.ideaSeverity) ideas
  pure hsParsedModule

ideaToWarnMsg :: GHC.DynFlags -> HLint.Idea -> GHC.WarnMsg
ideaToWarnMsg dynFlags idea =
  let
    mkErrMsg = case HLint.ideaSeverity idea of
      HLint.Error -> GHC.mkPlainErrMsg
      _ -> GHC.mkPlainWarnMsg
    srcSpan = HLint.ideaSpan idea
    msgDoc = ideaToMsgDoc idea
  in mkErrMsg dynFlags srcSpan msgDoc

ideaToMsgDoc :: HLint.Idea -> GHC.MsgDoc
ideaToMsgDoc idea = GHC.vcat
  [ GHC.text $ show (HLint.ideaSeverity idea) <> ": " <> HLint.ideaHint idea
  , maybe GHC.empty (GHC.text . mappend "Why not: ") $ HLint.ideaTo idea
  , GHC.vcat . fmap (GHC.text . mappend "Note: " . show) $ HLint.ideaNote idea
  ]
