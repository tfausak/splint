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

getSettings :: [String] -> GHC.Hsc Settings
getSettings options = do
  let insert = Stm.modifyTVar settingsTVar . Map.insert options
  remoteData <- io . stm $ do
    settings <- Stm.readTVar settingsTVar
    let remoteData = Map.findWithDefault NotAsked options settings
    case remoteData of
      NotAsked -> insert Loading
      _ -> pure ()
    pure remoteData
  case remoteData of
    NotAsked -> io . withTMVar settingsTMVar . const $ do
      result <- Exception.try $ HLint.argsSettings options
      case result of
        Left ioException -> do
          stm . insert $ Failure ioException
          Exception.throwIO ioException
        Right settings -> do
          stm . insert $ Success settings
          pure settings
    Loading -> do
      io $ Concurrent.threadDelay 1000
      getSettings options
    Failure ioException -> io $ Exception.throwIO ioException
    Success settings -> pure settings

io :: IO.MonadIO m => IO a -> m a
io = GHC.liftIO

stm :: Stm.STM a -> IO a
stm = Stm.atomically

withTMVar :: Stm.TMVar a -> (a -> IO b) -> IO b
withTMVar var =
  Exception.bracket (stm $ Stm.takeTMVar var) (stm . Stm.putTMVar var)

-- | Getting settings is not instantaneous. Since settings are usually reused
-- between modules, it makes sense to cache them. However each module can
-- potentially customize its settings using the @OPTIONS_GHC@ pragma. This
-- variable is used as a cache of settings keyed on the command line options.
settingsTVar
  :: Stm.TVar (Map.Map [String] (RemoteData Exception.IOException Settings))
settingsTVar = Unsafe.unsafePerformIO $ Stm.newTVarIO Map.empty
{-# NOINLINE settingsTVar #-}

-- | Even though we cache settings based on command line options, we only want
-- to load settings one at a time. Practically this is to work around a bug in
-- GHC. But aside from that, loading settings calls @withArgs@ and doing that
-- simultaneously in separate threads is dubious.
-- <https://gitlab.haskell.org/ghc/ghc/issues/18261>
settingsTMVar :: Stm.TMVar ()
settingsTMVar = Unsafe.unsafePerformIO $ Stm.newTMVarIO ()
{-# NOINLINE settingsTMVar #-}

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
