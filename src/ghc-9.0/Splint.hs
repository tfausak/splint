module Splint where

import qualified GHC.Data.Bag as GHC
import qualified GHC.Plugins as GHC
import qualified GHC.Utils.Error as GHC
import qualified Language.Haskell.HLint as HLint
import qualified Splint.Settings as Settings

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
action commandLineOptions modSummary hsParsedModule = do
  (parseFlags, classifies, hint) <- GHC.liftIO $ Settings.load commandLineOptions
  moduleEx <- parse parseFlags modSummary hsParsedModule
  dynFlags <- GHC.getDynFlags
  GHC.liftIO
    . GHC.printOrThrowWarnings dynFlags
    . GHC.listToBag
    . fmap (ideaToWarnMsg dynFlags)
    . filter ((/= HLint.Ignore) . HLint.ideaSeverity)
    $ HLint.applyHints classifies hint [moduleEx]
  pure hsParsedModule

ideaToWarnMsg :: GHC.DynFlags -> HLint.Idea -> GHC.WarnMsg
ideaToWarnMsg dynFlags idea =
  let
    mkErrMsg = case HLint.ideaSeverity idea of
      HLint.Error -> GHC.mkPlainErrMsg
      _ -> GHC.mkPlainWarnMsg
    srcSpan = case HLint.unpackSrcSpan $ HLint.ideaSpan idea of
      Nothing -> GHC.noSrcSpan
      Just (file, (startLine, startColumn), (endLine, endColumn)) ->
        GHC.mkSrcSpan
          (GHC.mkSrcLoc (GHC.mkFastString file) startLine startColumn)
          (GHC.mkSrcLoc (GHC.mkFastString file) endLine endColumn)
    msgDoc = ideaToMsgDoc idea
  in mkErrMsg dynFlags srcSpan msgDoc

ideaToMsgDoc :: HLint.Idea -> GHC.MsgDoc
ideaToMsgDoc idea = GHC.vcat
  [ GHC.text $ HLint.ideaHint idea
  , case HLint.ideaTo idea of
    Just to | not $ null to -> GHC.text $ "Perhaps: " <> to
    _ -> GHC.empty
  , GHC.vcat . fmap (GHC.text . mappend "Note: " . show) $ HLint.ideaNote idea
  ]

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
