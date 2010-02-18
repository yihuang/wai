{-# LANGUAGE Rank2Types #-}
-- | A collection of utility functions for dealing with 'Enumerator a's.
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
    --, fromEitherFile
    ) where

import Network.Wai (Enumerator, Iteratee, RecEnumerator, fix)
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString as B
import System.IO.Unsafe (unsafeInterleaveIO)
import System.IO (withBinaryFile, IOMode (ReadMode), Handle, hIsEOF)
import Data.ByteString.Lazy.Internal (defaultChunkSize)
import Control.Concurrent (forkIO)
import Control.Concurrent.MVar

-- | Performs a specified conversion on each 'B.ByteString' output by an
-- enumerator.
mapE :: (B.ByteString -> B.ByteString) -> Enumerator a -> Enumerator a
mapE f e rec iter = e rec iter' where
    iter' a = iter a . f

-- | This uses 'unsafeInterleaveIO' to lazily read from an enumerator. All
-- normal lazy I/O warnings apply.
toLBS :: Enumerator Stream -> IO L.ByteString
toLBS e = do
    stream <- toStream e
    L.fromChunks `fmap` helper stream
      where
        helper :: IOList -> IO [B.ByteString]
        helper (IOCons x iolist tocont) = unsafeInterleaveIO $ do
            swapMVar tocont False
            xs <- iolist >>= helper
            return $ x : xs
        helper Nil = return []

-- | This function safely converts a lazy bytestring into an enumerator.
fromLBS :: L.ByteString -> IO (Enumerator a)
fromLBS lbs = do
    mbslist <- newMVar $ L.toChunks lbs
    let k :: RecEnumerator a -> Iteratee a -> a -> IO (Either a a)
        k self iter a = do
                mbs <- modifyMVar mbslist helper
                case mbs of
                    Nothing -> return $ Right a
                    Just bs -> do
                        ea' <- iter a bs
                        case ea' of
                            Left a' -> return $ Left a'
                            Right a' -> self iter a'
    return k
    where
        helper [] = return ([], Nothing)
        helper (b:bs) = return (bs, Just b)

-- | Same as 'fromLBS', but the lazy bytestring is in the IO monad. This allows
-- you to lazily read a file into memory, perform some mapping on the data and
-- convert it into an enumerator.
fromLBS' :: IO L.ByteString -> IO (Enumerator a)
fromLBS' lbs = lbs >>= fromLBS

data Stream = Cons B.ByteString (IO Stream) (MVar Bool)
            | EmptyStream1 (MVar Bool)
            | EmptyStream2 (MVar Bool)
            | ElemStream B.ByteString (MVar Bool)
instance Show Stream where
    show (EmptyStream1 _) = "EmptyStream1"
    show (EmptyStream2 _) = "EmptyStream2"
    show (ElemStream bs _) = show ("ElemStream", bs)
    show (Cons bs _ _) = show ("Cons", bs)

data IOList = Nil | IOCons B.ByteString (IO IOList) (MVar Bool)
streamToIOList (Cons bs s b) = IOCons bs (fmap streamToIOList s) b
streamToIOList (EmptyStream2 _) = Nil
streamToIOList (ElemStream bs b) = IOCons bs (return Nil) b

toStream :: Enumerator Stream -> IO IOList
toStream f = do
  mtocont <- newMVar True
  fmap (either streamToIOList streamToIOList) $ f recf go2 (EmptyStream1 mtocont) where
    recf :: RecEnumerator Stream
    recf iter (ElemStream c mtocont) = do
        tocont <- readMVar mtocont
        if tocont
            then do
                let next = either id id `fmap` f recf iter (EmptyStream2 mtocont)
                return $ Right $ Cons c next mtocont
            else return $ Left $ ElemStream c mtocont
    go2 :: Iteratee Stream
    go2 (EmptyStream1 b) bs = do
        return $ Right $ ElemStream bs b
    go2 (EmptyStream2 b) bs = do
        return $ Right $ ElemStream bs b
    go2 s bs = print ("go2", s, bs) >> error "go2"

unRight (Right x) = x

-- | A source is a more standard way of accessing data from an 'Enumerator a'.
-- Each time you call it, it returns the next chunk of data if available, or
-- 'Nothing' if the data has been completely consumed.
toSource :: Enumerator () -> IO (IO (Maybe B.ByteString))
toSource e = do
    buffer <- newEmptyMVar
    _ <- forkIO $ fix e (helper buffer) () >> putMVar buffer Nothing
    return $ source buffer
      where
        helper :: MVar (Maybe B.ByteString)
               -> ()
               -> B.ByteString
               -> IO (Either () ())
        helper buffer _ bs = do
            putMVar buffer $ Just bs
            return $ Right ()
        source :: MVar (Maybe B.ByteString) -> IO (Maybe B.ByteString)
        source mmbs = do
            mbs <- takeMVar mmbs
            case mbs of
                Nothing -> do
                    -- By putting Nothing back in, the source can be called
                    -- again without causing a deadlock.
                    putMVar mmbs Nothing
                    return Nothing
                Just bs -> return $ Just bs

-- | Read a chunk of data from the given 'Handle' at a time. We use
-- 'defaultChunkSize' from the bytestring package to determine the largest
-- chunk to take.
fromHandle :: Handle -> Enumerator a
fromHandle h self iter a = do
    eof <- hIsEOF h
    if eof
        then return $ Right a
        else do
            bs <- B.hGet h defaultChunkSize
            ea' <- iter a bs
            case ea' of
                Left a' -> return $ Left a'
                Right a' -> self iter a'

-- | A little wrapper around 'fromHandle' which first opens a file for reading.
fromFile :: FilePath -> Enumerator a
fromFile fp self iter a0 = withBinaryFile fp ReadMode $ \h
    -> fromHandle h self iter a0

-- | Since the response body is defined as an 'Either' 'FilePath' 'Enumerator a',
-- this function simply reduces the whole operator to an enumerator. This can
-- be convenient for server implementations not optimizing file sending.
--fromEitherFile :: Either FilePath Enumerator a -> Enumerator a
fromEitherFile = either fromFile id
