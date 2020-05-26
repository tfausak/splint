module Splint
  ( plugin
  )
where

import qualified Bag as GHC
import qualified Data.IORef as IORef
import qualified ErrUtils as GHC
import qualified GhcPlugins as GHC
import qualified Language.Haskell.HLint as HLint
import qualified Splint.Parser as Splint
import qualified System.IO.Unsafe as Unsafe

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
  (parseFlags, classifies, hint) <- getSettings commandLineOptions
  moduleEx <- Splint.parse parseFlags modSummary hsParsedModule
  dynFlags <- GHC.getDynFlags
  GHC.liftIO
    . GHC.printOrThrowWarnings dynFlags
    . GHC.listToBag
    . fmap (ideaToWarnMsg dynFlags)
    . filter ((/= HLint.Ignore) . HLint.ideaSeverity)
    $ HLint.applyHints classifies hint [moduleEx]
  pure hsParsedModule

type Settings = (HLint.ParseFlags, [HLint.Classify], HLint.Hint)

getSettings :: [GHC.CommandLineOption] -> GHC.Hsc Settings
getSettings commandLineOptions = GHC.liftIO $ do
  maybeSettings <- IORef.readIORef settingsRef
  case maybeSettings of
    Just settings -> pure settings
    Nothing -> do
      settings <- HLint.argsSettings commandLineOptions
      IORef.writeIORef settingsRef $ Just settings
      pure settings

{-# NOINLINE settingsRef #-}
settingsRef :: IORef.IORef (Maybe Settings)
settingsRef = Unsafe.unsafePerformIO $ IORef.newIORef Nothing

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
