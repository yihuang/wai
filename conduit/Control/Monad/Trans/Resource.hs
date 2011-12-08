{-# LANGUAGE FlexibleContexts #-}
module Control.Monad.Trans.Resource
    ( -- * Data types
      ResourceT
    , ReleaseKey
      -- * Unwrap
    , runResourceT
      -- * Resource allocation
    , with
    , register
    , release
    ) where

import Control.Monad.Trans.Reader (ReaderT, runReaderT, ask)
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.Unique (newUnique, hashUnique)
import Control.Exception.Lifted (try, finally, SomeException, mask, mask_)
import Control.Monad.Trans.Control (MonadBaseControl)
import qualified Data.IORef as I
import Control.Monad.Base (MonadBase, liftBase)

newtype ReleaseMap m = ReleaseMap (IntMap (ResourceT m ()))
newtype ReleaseKey = ReleaseKey Int

type ResourceT m = ReaderT (I.IORef (ReleaseMap m)) m

with :: MonadBaseControl IO m
     => ResourceT m a -- ^ allocate
     -> (a -> ResourceT m ()) -- ^ free resource
     -> ResourceT m (ReleaseKey, a)
with acquire rel = mask_ $ do
    a <- acquire
    key <- register $ rel a
    return (key, a)

modify :: MonadBase IO m => (ReleaseMap m -> ReleaseMap m) -> ResourceT m ()
modify f = do
    istate <- ask
    liftBase $ I.atomicModifyIORef istate $ \a -> (f a, ())

get :: MonadBase IO m => ResourceT m (ReleaseMap m)
get = ask >>= liftBase . I.readIORef

register :: MonadBaseControl IO m
         => ResourceT m ()
         -> ResourceT m ReleaseKey
register rel = mask_ $ do
    key <- liftBase $ fmap hashUnique newUnique
    modify (insert key rel)
    return (ReleaseKey key)
  where
    insert k v (ReleaseMap m) = ReleaseMap $ IntMap.insert k v m

release :: MonadBaseControl IO m
        => ReleaseKey
        -> ResourceT m ()
release (ReleaseKey key) = mask $ \restore -> do
    ReleaseMap m <- get
    case IntMap.lookup key m of
        Nothing -> return () -- maybe we should throw an exception?
        Just r -> modify (delete key) >> restore (try' r >> return ())
  where
    delete k (ReleaseMap m) = ReleaseMap $ IntMap.delete k m

runResourceT :: MonadBaseControl IO m
          => ResourceT m a
          -> m a
runResourceT r = do
    istate <- liftBase $ I.newIORef $ ReleaseMap IntMap.empty
    runReaderT (r `finally` cleanup) istate
  where
    cleanup = do
        ReleaseMap m <- get
        if IntMap.null m
            then return ()
            else do
                let (key, _) = IntMap.findMin m
                release $ ReleaseKey key
                cleanup

try' :: MonadBaseControl IO m
     => m a
     -> m (Either SomeException a)
try' = try
