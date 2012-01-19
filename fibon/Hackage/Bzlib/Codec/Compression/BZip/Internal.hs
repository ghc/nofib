{-# LANGUAGE CPP #-}
-----------------------------------------------------------------------------
-- |
-- Copyright   :  (c) 2006-2008 Duncan Coutts
-- License     :  BSD-style
--
-- Maintainer  :  duncan@haskell.org
-- Stability   :  provisional
-- Portability :  portable (H98 + FFI)
--
-- Pure stream based interface to lower level bzlib wrapper
--
-----------------------------------------------------------------------------
module Codec.Compression.BZip.Internal (

  -- * Compression
  compress,
  CompressParams(..),
  defaultCompressParams,

  -- * Decompression
  decompress,
  DecompressParams(..),
  defaultDecompressParams,

  -- * The compression parameter types
  Stream.BlockSize(..),
  Stream.WorkFactor(..),
  Stream.MemoryLevel(..),
  ) where

import Prelude hiding (length)
import Control.Monad (when)
import Control.Exception (assert)
import qualified Data.ByteString.Lazy as L
#ifdef BYTESTRING_IN_BASE
import qualified Data.ByteString.Base as S
#else
import qualified Data.ByteString.Lazy.Internal as L
import qualified Data.ByteString.Internal as S
#endif

import qualified Codec.Compression.BZip.Stream as Stream
import Codec.Compression.BZip.Stream (Stream)

-- | The full set of parameters for compression. The defaults are
-- 'defaultCompressParams'.
--
-- The 'compressBufferSize' is the size of the first output buffer containing
-- the compressed data. If you know an approximate upper bound on the size of
-- the compressed data then setting this parameter can save memory. The default
-- compression output buffer size is @16k@. If your extimate is wrong it does
-- not matter too much, the default buffer size will be used for the remaining
-- chunks.
--
data CompressParams = CompressParams {
  compressBlockSize   :: Stream.BlockSize,
  compressWorkFactor  :: Stream.WorkFactor,
  compressBufferSize  :: Int
}

-- | The full set of parameters for decompression. The defaults are
-- 'defaultDecompressParams'.
--
-- The 'decompressBufferSize' is the size of the first output buffer,
-- containing the uncompressed data. If you know an exact or approximate upper
-- bound on the size of the decompressed data then setting this parameter can
-- save memory. The default decompression output buffer size is @32k@. If your
-- extimate is wrong it does not matter too much, the default buffer size will
-- be used for the remaining chunks.
--
-- One particular use case for setting the 'decompressBufferSize' is if you
-- know the exact size of the decompressed data and want to produce a strict
-- 'Data.ByteString.ByteString'. The compression and deccompression functions
-- use lazy 'Data.ByteString.Lazy.ByteString's but if you set the
-- 'decompressBufferSize' correctly then you can generate a lazy
-- 'Data.ByteString.Lazy.ByteString' with exactly one chunk, which can be
-- converted to a strict 'Data.ByteString.ByteString' in @O(1)@ time using
-- @'Data.ByteString.concat' . 'Data.ByteString.Lazy.toChunks'@.
--
data DecompressParams = DecompressParams {
  decompressMemoryLevel :: Stream.MemoryLevel,
  decompressBufferSize  :: Int
}

-- | The default set of parameters for compression. This is typically used with
-- the @compressWith@ function with specific paramaters overridden.
--
defaultCompressParams :: CompressParams
defaultCompressParams = CompressParams {
  compressBlockSize   = Stream.DefaultBlockSize,
  compressWorkFactor  = Stream.DefaultWorkFactor,
  compressBufferSize  = defaultCompressBufferSize
}

-- | The default set of parameters for decompression. This is typically used with
-- the @compressWith@ function with specific paramaters overridden.
--
defaultDecompressParams :: DecompressParams
defaultDecompressParams = DecompressParams {
  decompressMemoryLevel = Stream.DefaultMemoryLevel,
  decompressBufferSize  = defaultDecompressBufferSize
}

-- | The default chunk sizes for the output of compression and decompression
-- are 16k and 32k respectively (less a small accounting overhead).
--
defaultCompressBufferSize, defaultDecompressBufferSize :: Int
#ifdef BYTESTRING_IN_BASE
defaultCompressBufferSize   = 16 * 1024 - 16
defaultDecompressBufferSize = 32 * 1024 - 16
#else
defaultCompressBufferSize   = 16 * 1024 - L.chunkOverhead
defaultDecompressBufferSize = 32 * 1024 - L.chunkOverhead
#endif

{-# NOINLINE compress #-}
compress
  :: CompressParams
  -> L.ByteString
  -> L.ByteString
compress (CompressParams blockSize workFactor initChunkSize) input =
  L.fromChunks $ Stream.run $ do
    Stream.compressInit blockSize Stream.Silent workFactor
    case L.toChunks input of
      [] -> fillBuffers 14 [] --bzip2 header is 14 bytes
      S.PS inFPtr offset length : chunks -> do
        Stream.pushInputBuffer inFPtr offset length
        fillBuffers initChunkSize chunks

  where
    -- we flick between two states:
    --   * where one or other buffer is empty
    --       - in which case we refill one or both
    --   * where both buffers are non-empty
    --       - in which case we compress until a buffer is empty

  fillBuffers :: Int
              -> [S.ByteString]
              -> Stream [S.ByteString]
  fillBuffers outChunkSize inChunks = do
    Stream.consistencyCheck

    -- in this state there are two possabilities:
    --   * no outbut buffer space is available
    --       - in which case we must make more available
    --   * no input buffer is available
    --       - in which case we must supply more
    inputBufferEmpty <- Stream.inputBufferEmpty
    outputBufferFull <- Stream.outputBufferFull

    assert (inputBufferEmpty || outputBufferFull) $ return ()

    when outputBufferFull $ do
      outFPtr <- Stream.unsafeLiftIO (S.mallocByteString outChunkSize)
      Stream.pushOutputBuffer outFPtr 0 outChunkSize

    if inputBufferEmpty
      then case inChunks of
             [] -> drainBuffers []
             S.PS inFPtr offset length : inChunks' -> do
                Stream.pushInputBuffer inFPtr offset length
                drainBuffers inChunks'
      else drainBuffers inChunks


  drainBuffers ::
      [S.ByteString]
   -> Stream [S.ByteString]
  drainBuffers inChunks = do

    inputBufferEmpty' <- Stream.inputBufferEmpty
    outputBufferFull' <- Stream.outputBufferFull
    assert(not outputBufferFull'
       && (null inChunks || not inputBufferEmpty')) $ return ()
    -- this invariant guarantees we can always make forward progress

    let action = if null inChunks then Stream.Finish else Stream.Run
    status <- Stream.compress action

    case status of
      Stream.Ok -> do
        outputBufferFull <- Stream.outputBufferFull
        if outputBufferFull
          then do (outFPtr, offset, length) <- Stream.popOutputBuffer
                  outChunks <- Stream.unsafeInterleave
                    (fillBuffers defaultCompressBufferSize inChunks)
                  return (S.PS outFPtr offset length : outChunks)
          else do fillBuffers defaultCompressBufferSize inChunks

      Stream.StreamEnd -> do
        inputBufferEmpty <- Stream.inputBufferEmpty
        assert inputBufferEmpty $ return ()
        outputBufferBytesAvailable <- Stream.outputBufferBytesAvailable
        if outputBufferBytesAvailable > 0
          then do (outFPtr, offset, length) <- Stream.popOutputBuffer
                  Stream.finalise
                  return [S.PS outFPtr offset length]
          else do Stream.finalise
                  return []


{-# NOINLINE decompress #-}
decompress
  :: DecompressParams
  -> L.ByteString
  -> L.ByteString
decompress (DecompressParams memLevel initChunkSize) input =
  L.fromChunks $ Stream.run $ do
    Stream.decompressInit Stream.Silent memLevel
    case L.toChunks input of
      [] -> fillBuffers 4 [] --always an error anyway
      S.PS inFPtr offset length : chunks -> do
        Stream.pushInputBuffer inFPtr offset length
        fillBuffers initChunkSize chunks

  where
    -- we flick between two states:
    --   * where one or other buffer is empty
    --       - in which case we refill one or both
    --   * where both buffers are non-empty
    --       - in which case we compress until a buffer is empty

  fillBuffers :: Int
              -> [S.ByteString]
              -> Stream [S.ByteString]
  fillBuffers outChunkSize inChunks = do

    -- in this state there are two possabilities:
    --   * no outbut buffer space is available
    --       - in which case we must make more available
    --   * no input buffer is available
    --       - in which case we must supply more
    inputBufferEmpty <- Stream.inputBufferEmpty
    outputBufferFull <- Stream.outputBufferFull

    assert (inputBufferEmpty || outputBufferFull) $ return ()

    when outputBufferFull $ do
      outFPtr <- Stream.unsafeLiftIO (S.mallocByteString outChunkSize)
      Stream.pushOutputBuffer outFPtr 0 outChunkSize

    if inputBufferEmpty
      then case inChunks of
             [] -> drainBuffers []
             S.PS inFPtr offset length : inChunks' -> do
                Stream.pushInputBuffer inFPtr offset length
                drainBuffers inChunks'
      else drainBuffers inChunks


  drainBuffers ::
      [S.ByteString]
   -> Stream [S.ByteString]
  drainBuffers inChunks = do

    inputBufferEmpty' <- Stream.inputBufferEmpty
    outputBufferFull' <- Stream.outputBufferFull
    assert(not outputBufferFull'
       && (null inChunks || not inputBufferEmpty')) $ return ()
    -- this invariant guarantees we can always make forward progress or at
    -- least detect premature EOF

    status <- Stream.decompress

    case status of
      Stream.Ok -> do
        outputBufferFull <- Stream.outputBufferFull
        if outputBufferFull
          then do (outFPtr, offset, length) <- Stream.popOutputBuffer
                  outChunks <- Stream.unsafeInterleave
                    (fillBuffers defaultDecompressBufferSize inChunks)
                  return (S.PS outFPtr offset length : outChunks)
          else do -- We need to detect if we ran out of input:
                  inputBufferEmpty <- Stream.inputBufferEmpty
                  if inputBufferEmpty && null inChunks
                    then fail "premature end of compressed stream"
                    else fillBuffers defaultDecompressBufferSize inChunks

      Stream.StreamEnd -> do
        -- Note that there may be input bytes still available if the stream
        -- is embeded in some other data stream. Here we just silently discard
        -- any trailing data.
        outputBufferBytesAvailable <- Stream.outputBufferBytesAvailable
        if outputBufferBytesAvailable > 0
          then do (outFPtr, offset, length) <- Stream.popOutputBuffer
                  Stream.finalise
                  return [S.PS outFPtr offset length]
          else do Stream.finalise
                  return []
