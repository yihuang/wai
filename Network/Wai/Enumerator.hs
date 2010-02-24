{-# LANGUAGE Rank2Types #-}
-- | A collection of utility functions for dealing with 'Enumerator's.
module Network.Wai.Enumerator
    ( -- * Utilities
      mapE
      -- * Conversions
    , -- ** Lazy byte strings
      toLBS
    , fromLBS
    , fromLBS'
      -- ** Source
    , toSource
      -- ** Handle
    , fromHandle
      -- ** FilePath
    , fromFile
    , fromEitherFile
    ) where

import Network.Wai (Enumerator (..), Iteratee)
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString as B
import System.IO.Unsafe (unsafeInterleaveIO)
import System.IO (withBinaryFile, IOMode (ReadMode), Handle, hIsEOF)
import Data.ByteString.Lazy.Internal (defaultChunkSize)
import Control.Concurrent.MVar

-- | Performs a specified conversion on each 'B.ByteString' output by an
-- enumerator.
mapE :: (B.ByteString -> B.ByteString) -> Enumerator -> Enumerator
mapE f (Enumerator e) = Enumerator $ \rec iter -> e rec (iter' iter) where
    iter' iter a = iter a . f

-- | This uses 'unsafeInterleaveIO' to lazily read from an enumerator. All
-- normal lazy I/O warnings apply.
toLBS :: Enumerator -> IO L.ByteString
toLBS e = fmap L.fromChunks (toStream e >>= go) where
    go :: Stream -> IO [B.ByteString]
    go Nil = return []
    go (Cons bs rest) = do
        rest' <- unsafeInterleaveIO $ rest >>= go
        return $ bs : rest'
    go _ = error "Invalid Stream in toLBS"

-- | This function safely converts a lazy bytestring into an enumerator.
fromLBS :: L.ByteString -> IO Enumerator
fromLBS lbs = do
  mbss <- newMVar $ L.toChunks lbs
  return $ Enumerator $ go mbss where
    go mbss rec iter a = do
      res <- modifyMVar mbss $ go' iter a
      case res of
        Left x -> return x
        Right x -> rec iter x
    go' _ a [] = return $ ([], Left $ Right a)
    go' iter a (x:xs) = do
        ea <- iter a x
        case ea of
            Left a' -> return $ (xs, Left $ Left a')
            Right a' -> return $ (xs, Right a')

-- | Same as 'fromLBS', but the lazy bytestring is in the IO monad. This allows
-- you to lazily read a file into memory, perform some mapping on the data and
-- convert it into an enumerator.
fromLBS' :: IO L.ByteString -> Enumerator
fromLBS' lbs' = Enumerator $ \rec iter a0 -> lbs' >>= \lbs -> do
    Enumerator enum <- fromLBS lbs
    enum rec iter a0

-- | A source is a more standard way of accessing data from an 'Enumerator'.
-- Each time you call it, it returns the next chunk of data if available, or
-- 'Nothing' if the data has been completely consumed.
toSource :: Enumerator -> IO (IO (Maybe B.ByteString))
toSource e = do
    mstream <- toStream e >>= newMVar
    return $ go mstream
      where
        go :: MVar Stream -> IO (Maybe B.ByteString)
        go mstream = modifyMVar mstream go'
        go' :: Stream -> IO (Stream, Maybe B.ByteString)
        go' Nil = return (Nil, Nothing)
        go' (Cons bs rest) = do
            rest' <- rest
            return (rest', Just bs)
        go' _ = error "Invalid Stream in toSource"

data Stream = Nil
            | Elem B.ByteString
            | Cons B.ByteString (IO Stream)
toStream :: Enumerator -> IO Stream
toStream (Enumerator enum) = fmap (either id id) $ enum rec iter Nil where
    iter :: Iteratee Stream
    iter _ c = return $ Right $ Elem c
    rec :: Iteratee Stream -> Stream -> IO (Either Stream Stream)
    rec _ Nil = return $ Right Nil
    rec iter' (Elem c) = return $ Right $ Cons c
                      $ fmap (either id id) $ enum rec iter' Nil
    rec _ _ = error "Invalid Stream to toStream.rec"

-- | Read a chunk of data from the given 'Handle' at a time. We use
-- 'defaultChunkSize' from the bytestring package to determine the largest
-- chunk to take.
fromHandle :: Handle -> Enumerator
fromHandle h = Enumerator $ \rec iter a -> do
    eof <- hIsEOF h
    if eof
        then return $ Right a
        else do
            bs <- B.hGet h defaultChunkSize
            ea' <- iter a bs
            case ea' of
                Left a' -> return $ Left a'
                Right a' -> rec iter a'

-- | A little wrapper around 'fromHandle' which first opens a file for reading.
fromFile :: FilePath -> Enumerator
fromFile fp = Enumerator $ \rec iter a0 -> withBinaryFile fp ReadMode $
    \h -> runEnumerator (fromHandle h) rec iter a0

-- | Since the response body is defined as an 'Either' 'FilePath' 'Enumerator',
-- this function simply reduces the whole operator to an enumerator. This can
-- be convenient for server implementations not optimizing file sending.
fromEitherFile :: Either FilePath Enumerator -> Enumerator
fromEitherFile = either fromFile id
