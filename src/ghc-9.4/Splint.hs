module Splint where

import qualified GHC.Data.Bag as Bag
import qualified GHC.Driver.Errors as Errors
import qualified GHC.Hs as Hs
import qualified GHC.Plugins as Plugins
import qualified GHC.Types.Error as Error
import qualified GHC.Utils.Logger as Logger
import qualified Language.Haskell.HLint as HLint
import qualified Splint.Settings as Settings

plugin :: Plugins.Plugin
plugin = Plugins.defaultPlugin
  { Plugins.parsedResultAction = parsedResultAction
  , Plugins.pluginRecompile = Plugins.purePlugin
  }

parsedResultAction
  :: [Plugins.CommandLineOption]
  -> Plugins.ModSummary
  -> Plugins.ParsedResult
  -> Plugins.Hsc Plugins.ParsedResult
parsedResultAction commandLineOptions _modSummary parsedResult = do
  (_parseFlags, classifies, hint) <- Plugins.liftIO
    $ Settings.load commandLineOptions
  let _ = id
        . HLint.applyHints classifies hint
        . pure
        . HLint.createModuleEx
        . _
        . Hs.hpm_module
        $ Plugins.parsedResultModule parsedResult
  {-
  logger <- Logger.getLogger
  dynFlags <- Plugins.getDynFlags
  Plugins.liftIO
    . Errors.printOrThrowWarnings logger dynFlags
    . Bag.listToBag
    . fmap ideaToWarnMsg
    . filter ((/=) HLint.Ignore . HLint.ideaSeverity)
    . HLint.applyHints classifies hint
    . pure
    . HLint.createModuleEx
    . Hs.hpm_module
    $ Hs.parsedResultModule parsedResult
  -}
  pure parsedResult { Plugins.parsedResultMessages = Plugins.parsedResultMessages parsedResult }
{-
ideaToWarnMsg :: HLint.Idea -> Error.WarnMsg
ideaToWarnMsg idea =
  Error.mkPlainWarnMsg (ideaToSrcSpan idea) (ideaToSDoc idea)

ideaToSrcSpan :: HLint.Idea -> Plugins.SrcSpan
ideaToSrcSpan idea = case HLint.unpackSrcSpan $ HLint.ideaSpan idea of
  Nothing -> Plugins.noSrcSpan
  Just (filePath, (startLine, startColumn), (endLine, endColumn)) ->
    let fastString = Plugins.mkFastString filePath
    in
      Plugins.mkSrcSpan
        (Plugins.mkSrcLoc fastString startLine startColumn)
        (Plugins.mkSrcLoc fastString endLine endColumn)

ideaToSDoc :: HLint.Idea -> Error.SDoc
ideaToSDoc idea = Plugins.vcat
  [ Plugins.text $ HLint.ideaHint idea
  , case HLint.ideaTo idea of
    Just to | not $ null to -> Plugins.text $ "Perhaps: " <> to
    _ -> Plugins.empty
  , Plugins.vcat
  . fmap (Plugins.text . mappend "Note: " . show)
  $ HLint.ideaNote idea
  ]
-}
