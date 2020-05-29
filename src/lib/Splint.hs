module Splint
  ( plugin
  )
where

import qualified Bag as GHC
import qualified Control.Concurrent as Concurrent
import qualified Control.Concurrent.STM as Stm
import qualified Control.Exception as Exception
import qualified Control.Monad.IO.Class as IO
import qualified Data.Map as Map
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
  io
    . GHC.printOrThrowWarnings dynFlags
    . GHC.listToBag
    . fmap (ideaToWarnMsg dynFlags)
    . filter ((/= HLint.Ignore) . HLint.ideaSeverity)
    $ HLint.applyHints classifies hint [moduleEx]
  pure hsParsedModule

type Settings = (HLint.ParseFlags, [HLint.Classify], HLint.Hint)

getSettings :: [GHC.CommandLineOption] -> GHC.Hsc Settings
getSettings commandLineOptions = do
  remoteData <- io . stm $ do
    settings <- Stm.readTVar settingsTVar
    let remoteData = Map.findWithDefault NotAsked commandLineOptions settings
    case remoteData of
      NotAsked ->
        Stm.modifyTVar settingsTVar $ Map.insert commandLineOptions Loading
      _ -> pure ()
    pure remoteData
  case remoteData of
    NotAsked -> io . withTMVar settingsTMVar . const $ do
      result <- Exception.try $ HLint.argsSettings commandLineOptions
      case result of
        Left ioException -> do
          stm
            . Stm.modifyTVar settingsTVar
            . Map.insert commandLineOptions
            $ Failure ioException
          Exception.throwIO ioException
        Right settings -> do
          stm
            . Stm.modifyTVar settingsTVar
            . Map.insert commandLineOptions
            $ Success settings
          pure settings
    Loading -> do
      io $ Concurrent.threadDelay 1000
      getSettings commandLineOptions
    Failure ioException -> io $ Exception.throwIO ioException
    Success settings -> pure settings

io :: IO.MonadIO m => IO a -> m a
io = GHC.liftIO

stm :: Stm.STM a -> IO a
stm = Stm.atomically

withTMVar :: Stm.TMVar a -> (a -> IO b) -> IO b
withTMVar var =
  Exception.bracket (stm $ Stm.takeTMVar var) (stm . Stm.putTMVar var)

{-# NOINLINE settingsTVar #-}
settingsTVar
  :: Stm.TVar
       ( Map.Map
           [GHC.CommandLineOption]
           (RemoteData Exception.IOException Settings)
       )
settingsTVar = Unsafe.unsafePerformIO $ Stm.newTVarIO Map.empty

{-# NOINLINE settingsTMVar #-}
settingsTMVar :: Stm.TMVar ()
settingsTMVar = Unsafe.unsafePerformIO $ Stm.newTMVarIO ()

data RemoteData e a
  = NotAsked
  | Loading
  | Failure e
  | Success a
  deriving (Eq, Show)

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
