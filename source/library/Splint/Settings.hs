module Splint.Settings where

import qualified Control.Concurrent.STM as Stm
import qualified Control.Exception as Exception
import qualified Data.Map as Map
import qualified Language.Haskell.HLint as HLint
import qualified Splint.RemoteData as RemoteData
import qualified System.IO as IO
import qualified System.IO.Unsafe as Unsafe

type Settings = ([HLint.Classify], HLint.Hint)

-- | Getting settings is not instantaneous. Since settings are usually reused
-- between modules, it makes sense to cache them. However each module can
-- potentially customize its settings using the @OPTIONS_GHC@ pragma. This
-- variable is used as a cache of settings keyed on the command line options.
cache ::
  Stm.TVar
    (Map.Map [String] (RemoteData.RemoteData Exception.IOException Settings))
cache = Unsafe.unsafePerformIO $ Stm.newTVarIO Map.empty
{-# NOINLINE cache #-}

-- | Even though we cache settings based on command line options, we only want
-- to load settings one at a time. Practically this is to work around a bug in
-- GHC. But aside from that, loading settings calls @withArgs@ and doing that
-- simultaneously in separate threads is dubious.
-- <https://gitlab.haskell.org/ghc/ghc/issues/18261>
semaphore :: Stm.TMVar ()
semaphore = Unsafe.unsafePerformIO $ Stm.newTMVarIO ()
{-# NOINLINE semaphore #-}

withTMVar :: Stm.TMVar a -> (a -> IO b) -> IO b
withTMVar x =
  Exception.bracket
    (Stm.atomically $ Stm.takeTMVar x)
    (Stm.atomically . Stm.putTMVar x)

load :: [String] -> IO Settings
load commandLineOptions = do
  remoteData <- Stm.atomically $ do
    settings <- Stm.readTVar cache
    let remoteData =
          Map.findWithDefault RemoteData.NotAsked commandLineOptions settings
    case remoteData of
      RemoteData.NotAsked ->
        Stm.modifyTVar cache $ Map.insert commandLineOptions RemoteData.Loading
      RemoteData.Loading -> Stm.retry
      _ -> pure ()
    pure remoteData
  case remoteData of
    RemoteData.NotAsked -> do
      IO.hPutStrLn IO.stderr $ "[splint] Loading settings for " <> show commandLineOptions <> " ..."
      result <-
        withTMVar semaphore
          . const
          . Exception.try
          $ HLint.argsSettings commandLineOptions
      case result of
        Left ioException -> do
          Stm.atomically
            . Stm.modifyTVar cache
            . Map.insert commandLineOptions
            $ RemoteData.Failure ioException
          Exception.throwIO ioException
        Right (_, classifies, hint) -> do
          Stm.atomically
            . Stm.modifyTVar cache
            . Map.insert commandLineOptions
            $ RemoteData.Success (classifies, hint)
          pure (classifies, hint)
    RemoteData.Loading -> load commandLineOptions
    RemoteData.Failure ioException -> Exception.throwIO ioException
    RemoteData.Success settings -> pure settings
