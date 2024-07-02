module Splint where

import qualified Data.Maybe as Maybe
import qualified GHC.Data.Bag as Bag
import qualified GHC.Driver.Config.Diagnostic as Config
import qualified GHC.Driver.Errors
import qualified GHC.Driver.Errors.Types
import qualified GHC.Hs as Hs
import qualified GHC.Plugins as Plugin
import qualified GHC.Types.Error as Error
import qualified GHC.Utils.Logger as Logger
import qualified Language.Haskell.HLint as HLint
import qualified Splint.Replacement as Replacement
import qualified Splint.Settings as Settings

plugin :: Plugin.Plugin
plugin =
  Plugin.defaultPlugin
    { Plugin.parsedResultAction = parsedResultAction,
      Plugin.pluginRecompile = Plugin.purePlugin
    }

parsedResultAction ::
  [Plugin.CommandLineOption] ->
  modSummary ->
  Plugin.ParsedResult ->
  Plugin.Hsc Plugin.ParsedResult
parsedResultAction commandLineOptions _modSummary parsedResult = do
  logger <- Logger.getLogger
  dynFlags <- Plugin.getDynFlags
  Plugin.liftIO $ do
    (_parseFlags, classifies, hint) <- Settings.load commandLineOptions
    GHC.Driver.Errors.printOrThrowDiagnostics
      logger
      (Config.initPrintConfig dynFlags)
      (Config.initDiagOpts dynFlags)
      . Error.mkMessages
      . Bag.listToBag
      . fmap ideaToWarnMsg
      . HLint.applyHints classifies hint
      . pure
      . HLint.createModuleEx
      . Hs.hpm_module
      $ Plugin.parsedResultModule parsedResult
  pure parsedResult

ideaToWarnMsg :: HLint.Idea -> GHC.Driver.Errors.Types.WarnMsg
ideaToWarnMsg idea =
  Error.MsgEnvelope
    { Error.errMsgContext = Plugin.neverQualify,
      Error.errMsgDiagnostic =
        GHC.Driver.Errors.Types.ghcUnknownMessage
          Error.DiagnosticMessage
            { Error.diagHints =
                fmap (Error.UnknownHint . Replacement.fromString)
                  . Maybe.maybeToList
                  $ HLint.ideaTo idea,
              Error.diagMessage =
                Error.mkDecorated $
                  Plugin.text
                    (HLint.ideaHint idea)
                    : fmap
                      (Plugin.text . mappend "Note: " . show)
                      (HLint.ideaNote idea),
              Error.diagReason = case HLint.ideaSeverity idea of
                HLint.Ignore -> Error.WarningWithoutFlag
                HLint.Suggestion -> Error.WarningWithoutFlag
                HLint.Warning -> Error.WarningWithoutFlag
                HLint.Error -> Error.ErrorWithoutFlag
            },
      Error.errMsgSeverity = case HLint.ideaSeverity idea of
        HLint.Ignore -> Error.SevIgnore
        HLint.Suggestion -> Error.SevWarning
        HLint.Warning -> Error.SevWarning
        HLint.Error -> Error.SevError,
      Error.errMsgSpan = HLint.ideaSpan idea
    }
