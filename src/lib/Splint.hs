module Splint ( plugin ) where

import qualified Bag as GHC
import qualified Data.IORef as IORef
import qualified ErrUtils as GHC
import qualified GhcPlugins as GHC
import qualified Language.Haskell.HLint as HLint
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
action commandLineOptions _modSummary hsParsedModule = do
  dynFlags <- GHC.getDynFlags
  GHC.liftIO $ do
    (_parseFlags, classifies, hint) <- getSettings commandLineOptions
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

type Settings = (HLint.ParseFlags, [HLint.Classify], HLint.Hint)

getSettings :: [GHC.CommandLineOption] -> IO Settings
getSettings commandLineOptions = do
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
    srcSpan = HLint.ideaSpan idea
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
