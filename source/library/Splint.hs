module Splint where

import qualified Data.Maybe as Maybe
import qualified GHC.Data.Bag
import qualified GHC.Driver.Config.Diagnostic
import qualified GHC.Driver.Errors
import qualified GHC.Driver.Errors.Types
import qualified GHC.Hs
import qualified GHC.Plugins
import qualified GHC.Types.Error
import qualified GHC.Utils.Error
import qualified GHC.Utils.Logger
import qualified Language.Haskell.HLint as HLint
import qualified Splint.Replacement as Replacement
import qualified Splint.Settings as Settings

plugin :: GHC.Plugins.Plugin
plugin =
  GHC.Plugins.defaultPlugin
    { GHC.Plugins.parsedResultAction = parsedResultAction,
      GHC.Plugins.pluginRecompile = GHC.Plugins.purePlugin
    }

parsedResultAction ::
  [GHC.Plugins.CommandLineOption] ->
  modSummary ->
  GHC.Plugins.ParsedResult ->
  GHC.Plugins.Hsc GHC.Plugins.ParsedResult
parsedResultAction commandLineOptions _modSummary parsedResult = do
  logger <- GHC.Utils.Logger.getLogger
  dynFlags <- GHC.Plugins.getDynFlags
  let ghcMessageOpts = GHC.Driver.Config.Diagnostic.initPrintConfig dynFlags
      diagOpts = GHC.Driver.Config.Diagnostic.initDiagOpts dynFlags
  GHC.Plugins.liftIO $ do
    settings <- Settings.load commandLineOptions
    GHC.Driver.Errors.printOrThrowDiagnostics logger ghcMessageOpts diagOpts
      . GHC.Types.Error.mkMessages
      . GHC.Data.Bag.listToBag
      . fmap (ideaToWarnMsg diagOpts)
      . uncurry HLint.applyHints settings
      . pure
      . HLint.createModuleEx
      . GHC.Hs.hpm_module
      $ GHC.Plugins.parsedResultModule parsedResult
  pure parsedResult

ideaToWarnMsg :: GHC.Utils.Error.DiagOpts -> HLint.Idea -> GHC.Driver.Errors.Types.WarnMsg
ideaToWarnMsg diagOpts idea =
  let srcSpan = HLint.ideaSpan idea
      ghcHints =
        fmap (GHC.Types.Error.UnknownHint . Replacement.fromString)
          . Maybe.maybeToList
          $ HLint.ideaTo idea
      decoratedSDoc =
        GHC.Types.Error.mkDecorated $
          GHC.Plugins.text
            (HLint.ideaHint idea)
            : fmap
              (GHC.Plugins.text . mappend "Note: " . show)
              (HLint.ideaNote idea)
      diagnosticReason = case HLint.ideaSeverity idea of
        HLint.Ignore -> GHC.Types.Error.WarningWithoutFlag
        HLint.Suggestion -> GHC.Types.Error.WarningWithoutFlag
        HLint.Warning -> GHC.Types.Error.WarningWithoutFlag
        HLint.Error -> GHC.Types.Error.ErrorWithoutFlag
      diagnosticMessage =
        GHC.Types.Error.DiagnosticMessage
          { GHC.Types.Error.diagHints = ghcHints,
            GHC.Types.Error.diagMessage = decoratedSDoc,
            GHC.Types.Error.diagReason = diagnosticReason
          }
      ghcMessage = GHC.Driver.Errors.Types.ghcUnknownMessage diagnosticMessage
   in GHC.Utils.Error.mkPlainMsgEnvelope diagOpts srcSpan ghcMessage
