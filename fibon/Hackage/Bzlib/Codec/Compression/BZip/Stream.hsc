{-# LANGUAGE ForeignFunctionInterface #-}
-----------------------------------------------------------------------------
-- |
-- Copyright   :  (c) 2006-2008 Duncan Coutts
-- License     :  BSD-style
--
-- Maintainer  :  duncan.coutts@worc.ox.ac.uk
-- Stability   :  experimental
-- Portability :  portable (H98 + FFI)
--
-- BZlib wrapper layer
--
-----------------------------------------------------------------------------
module Codec.Compression.BZip.Stream (

  -- * The Zlib state monad
  Stream,
  run,
  unsafeInterleave,
  unsafeLiftIO,
  finalise,

  -- * Initialisation
  compressInit, 
  decompressInit,

  -- ** Initialisation parameters
  BlockSize(..),
  WorkFactor(..),
  MemoryLevel(..),
  Verbosity(..),

  -- * The buisness
  compress,
  decompress,
  Status(..),
  Action(..),

  -- * Buffer management
  -- ** Input buffer
  pushInputBuffer,
  inputBufferEmpty,

  -- ** Output buffer
  pushOutputBuffer,
  popOutputBuffer,
  outputBufferBytesAvailable,
  outputBufferSpaceRemaining,
  outputBufferFull,

  -- * Debugging
  consistencyCheck,
  dump,
  trace,

  ) where

import Foreign
         ( Word8, Ptr, nullPtr, plusPtr, peekByteOff, pokeByteOff, mallocBytes
         , ForeignPtr, FinalizerPtr, newForeignPtr_, addForeignPtrFinalizer
         , finalizeForeignPtr, withForeignPtr, touchForeignPtr
         , unsafeForeignPtrToPtr, unsafePerformIO )
import Foreign.C
         ( CInt, CUInt )
#ifdef BYTESTRING_IN_BASE
import Data.ByteString.Base (nullForeignPtr)
#else
import Data.ByteString.Internal (nullForeignPtr)
#endif
import System.IO.Unsafe (unsafeInterleaveIO)
import System.IO (hPutStrLn, stderr)
import Control.Monad (liftM)
import Control.Exception (assert)

import Prelude hiding (length)

#include "bzlib.h"


pushInputBuffer :: ForeignPtr Word8 -> Int -> Int -> Stream ()
pushInputBuffer inBuf' offset length = do

  -- must not push a new input buffer if the last one is not used up
  inAvail <- getInAvail
  assert (inAvail == 0) $ return ()

  -- Now that we're setting a new input buffer, we can be sure that zlib no
  -- longer has a reference to the old one. Therefore this is the last point
  -- at which the old buffer had to be retained. It's safe to release now.
  inBuf <- getInBuf 
  unsafeLiftIO $ touchForeignPtr inBuf    

  -- now set the available input buffer ptr and length
  setInBuf   inBuf'
  setInAvail length
  setInNext  (unsafeForeignPtrToPtr inBuf' `plusPtr` offset)
  -- Note the 'unsafe'. We are passing the raw ptr inside inBuf' to zlib.
  -- To make this safe we need to hold on to the ForeignPtr for at least as
  -- long as zlib is using the underlying raw ptr.


inputBufferEmpty :: Stream Bool
inputBufferEmpty = getInAvail >>= return . (==0)


pushOutputBuffer :: ForeignPtr Word8 -> Int -> Int -> Stream ()
pushOutputBuffer outBuf' offset length = do

  --must not push a new buffer if there is still data in the old one
  outAvail <- getOutAvail
  assert (outAvail == 0) $ return ()
  -- Note that there may still be free space in the output buffer, that's ok,
  -- you might not want to bother completely filling the output buffer say if
  -- there's only a few free bytes left.

  outBuf <- getOutBuf
  unsafeLiftIO $ touchForeignPtr outBuf

  -- now set the available input buffer ptr and length
  setOutBuf  outBuf'
  setOutFree length
  setOutNext (unsafeForeignPtrToPtr outBuf' `plusPtr` offset)

  setOutOffset offset
  setOutAvail  0


-- get that part of the output buffer that is currently full
-- (might be 0, use outputBufferBytesAvailable to check)
-- this may leave some space remaining in the buffer, use
-- outputBufferSpaceRemaining to check.
popOutputBuffer :: Stream (ForeignPtr Word8, Int, Int)
popOutputBuffer = do

  outBuf    <- getOutBuf
  outOffset <- getOutOffset
  outAvail  <- getOutAvail

  -- there really should be something to pop, otherwise it's silly
  assert (outAvail > 0) $ return ()

  setOutOffset (outOffset + outAvail)
  setOutAvail  0

  return (outBuf, outOffset, outAvail)


-- this is the number of bytes available in the output buffer
outputBufferBytesAvailable :: Stream Int
outputBufferBytesAvailable = getOutAvail


-- you needen't get all the output immediately, you can continue until
-- there is no more output space available, this tells you that amount
outputBufferSpaceRemaining :: Stream Int
outputBufferSpaceRemaining = getOutFree


-- you only need to supply a new buffer when there is no more output buffer
-- space remaining
outputBufferFull :: Stream Bool
outputBufferFull = getOutFree >>= return . (==0)


-- you can only run this when the output buffer is not empty
-- you can run it when the input buffer is empty but it doesn't do anything
-- after running deflate either the output buffer will be full
-- or the input buffer will be empty (or both)
compress :: Action -> Stream Status
compress action = do

  outFree <- getOutFree

  -- deflate needs free space in the output buffer
  assert (outFree > 0) $ return ()

  result <- compress_ action
  outFree' <- getOutFree
    
  -- number of bytes of extra output there is available as a result of
  -- the call to deflate:
  let outExtra = outFree - outFree'
  
  outAvail <- getOutAvail
  setOutAvail (outAvail + outExtra)
  return result


decompress :: Stream Status
decompress = do

  outFree <- getOutFree

  -- inflate needs free space in the output buffer
  assert (outFree > 0) $ return ()

  result <- decompress_
  outFree' <- getOutFree

  -- number of bytes of extra output there is available as a result of
  -- the call to inflate:
  let outExtra = outFree - outFree'

  outAvail <- getOutAvail
  setOutAvail (outAvail + outExtra)
  return result


----------------------------
-- Stream monad
--

newtype Stream a = BZ {
    unZ :: ForeignPtr StreamState
        -> ForeignPtr Word8
        -> ForeignPtr Word8
        -> Int -> Int
        -> IO (ForeignPtr Word8
              ,ForeignPtr Word8
              ,Int, Int, a)
  }

instance Monad Stream where
  (>>=)  = thenZ
--  m >>= f = (m `thenZ` \a -> consistencyCheck `thenZ_` returnZ a) `thenZ` f
  (>>)   = thenZ_
  return = returnZ
  fail   = (finalise >>) . failZ

returnZ :: a -> Stream a
returnZ a = BZ $ \_ inBuf outBuf outOffset outLength ->
                  return (inBuf, outBuf, outOffset, outLength, a)
{-# INLINE returnZ #-}

thenZ :: Stream a -> (a -> Stream b) -> Stream b
thenZ (BZ m) f =
  BZ $ \stream inBuf outBuf outOffset outLength ->
    m stream inBuf outBuf outOffset outLength >>=
      \(inBuf', outBuf', outOffset', outLength', a) ->
        unZ (f a) stream inBuf' outBuf' outOffset' outLength'
{-# INLINE thenZ #-}

thenZ_ :: Stream a -> Stream b -> Stream b
thenZ_ (BZ m) f =
  BZ $ \stream inBuf outBuf outOffset outLength ->
    m stream inBuf outBuf outOffset outLength >>=
      \(inBuf', outBuf', outOffset', outLength', _) ->
        unZ f stream inBuf' outBuf' outOffset' outLength'
{-# INLINE thenZ_ #-}

failZ :: String -> Stream a
failZ msg = BZ (\_ _ _ _ _ -> fail ("Codec.Compression.BZip: " ++ msg))

{-# NOINLINE run #-}
run :: Stream a -> a
run (BZ m) = unsafePerformIO $ do
  ptr <- mallocBytes (#{const sizeof(bz_stream)})
  #{poke bz_stream, bzalloc}   ptr nullPtr
  #{poke bz_stream, bzfree}    ptr nullPtr
  #{poke bz_stream, opaque}    ptr nullPtr
  #{poke bz_stream, next_in}   ptr nullPtr
  #{poke bz_stream, next_out}  ptr nullPtr
  #{poke bz_stream, avail_in}  ptr (0 :: CUInt)
  #{poke bz_stream, avail_out} ptr (0 :: CUInt)
  stream <- newForeignPtr_ ptr
  (_,_,_,_,a) <- m stream nullForeignPtr nullForeignPtr 0 0
  return a

unsafeLiftIO :: IO a -> Stream a
unsafeLiftIO m = BZ $ \_stream inBuf outBuf outOffset outLength -> do
  a <- m
  return (inBuf, outBuf, outOffset, outLength, a)

-- It's unsafe because we discard the values here, so if you mutate anything
-- between running this and forcing the result then you'll get an inconsistent
-- stream state.
unsafeInterleave :: Stream a -> Stream a
unsafeInterleave (BZ m) = BZ $ \stream inBuf outBuf outOffset outLength -> do
  res <- unsafeInterleaveIO (m stream inBuf outBuf outOffset outLength)
  let select (_,_,_,_,a) = a
  return (inBuf, outBuf, outOffset, outLength, select res)

getStreamState :: Stream (ForeignPtr StreamState)
getStreamState = BZ $ \stream inBuf outBuf outOffset outLength -> do
  return (inBuf, outBuf, outOffset, outLength, stream)

getInBuf :: Stream (ForeignPtr Word8)
getInBuf = BZ $ \_stream inBuf outBuf outOffset outLength -> do
  return (inBuf, outBuf, outOffset, outLength, inBuf)

getOutBuf :: Stream (ForeignPtr Word8)
getOutBuf = BZ $ \_stream inBuf outBuf outOffset outLength -> do
  return (inBuf, outBuf, outOffset, outLength, outBuf)

getOutOffset :: Stream Int
getOutOffset = BZ $ \_stream inBuf outBuf outOffset outLength -> do
  return (inBuf, outBuf, outOffset, outLength, outOffset)

getOutAvail :: Stream Int
getOutAvail = BZ $ \_stream inBuf outBuf outOffset outLength -> do
  return (inBuf, outBuf, outOffset, outLength, outLength)

setInBuf :: ForeignPtr Word8 -> Stream ()
setInBuf inBuf = BZ $ \_stream _ outBuf outOffset outLength -> do
  return (inBuf, outBuf, outOffset, outLength, ())

setOutBuf :: ForeignPtr Word8 -> Stream ()
setOutBuf outBuf = BZ $ \_stream inBuf _ outOffset outLength -> do
  return (inBuf, outBuf, outOffset, outLength, ())

setOutOffset :: Int -> Stream ()
setOutOffset outOffset = BZ $ \_stream inBuf outBuf _ outLength -> do
  return (inBuf, outBuf, outOffset, outLength, ())

setOutAvail :: Int -> Stream ()
setOutAvail outLength = BZ $ \_stream inBuf outBuf outOffset _ -> do
  return (inBuf, outBuf, outOffset, outLength, ())

----------------------------
-- Debug stuff
--

trace :: String -> Stream ()
trace = unsafeLiftIO . hPutStrLn stderr

dump :: Stream ()
dump = do
  inNext  <- getInNext
  inAvail <- getInAvail

  outNext <- getOutNext
  outFree <- getOutFree
  outAvail <- getOutAvail
  outOffset <- getOutOffset

  unsafeLiftIO $ hPutStrLn stderr $
    "Stream {\n" ++
    "  inNext    = " ++ show inNext    ++ ",\n" ++
    "  inAvail   = " ++ show inAvail   ++ ",\n" ++
    "\n" ++
    "  outNext   = " ++ show outNext   ++ ",\n" ++
    "  outFree   = " ++ show outFree   ++ ",\n" ++
    "  outAvail  = " ++ show outAvail  ++ ",\n" ++
    "  outOffset = " ++ show outOffset ++ "\n"  ++
    "}"

  consistencyCheck

consistencyCheck :: Stream ()
consistencyCheck = do

  outBuf    <- getOutBuf
  outOffset <- getOutOffset
  outAvail  <- getOutAvail
  outNext   <- getOutNext

  let outBufPtr = unsafeForeignPtrToPtr outBuf

  assert (outBufPtr `plusPtr` (outOffset + outAvail) == outNext) $ return ()


----------------------------
-- zlib wrapper layer
--

data Status =
    Ok         -- ^ The requested action was completed successfully.
  | StreamEnd  -- ^ Compression of data was completed, or the logical stream
               --   end was detected during decompression.

toStatus :: CInt -> Status
toStatus (#{const BZ_OK})         = Ok
toStatus (#{const BZ_RUN_OK})     = Ok
toStatus (#{const BZ_FLUSH_OK})   = Ok
toStatus (#{const BZ_FINISH_OK})  = Ok
toStatus (#{const BZ_STREAM_END}) = StreamEnd
toStatus other = error ("unexpected bzip2 status: " ++ show other)

failIfError :: CInt -> Stream ()
failIfError errno
  | errno >= 0 = return ()
  | otherwise  = fail (getErrorMessage errno)

getErrorMessage :: CInt -> String
getErrorMessage errno = case errno of
 #{const BZ_SEQUENCE_ERROR}   -> "incorrect sequence of calls"
 #{const BZ_PARAM_ERROR}      -> "incorrect parameter"
 #{const BZ_MEM_ERROR}        -> "not enough memory"
 #{const BZ_DATA_ERROR}       -> "compressed data stream is corrupt"
 #{const BZ_DATA_ERROR_MAGIC} -> "data stream is not a bzip2 file"
 #{const BZ_CONFIG_ERROR}     -> "configuration error in bzip2 lib"
 other                        -> "unknown or impossible error code: "
                              ++ show other

data Action =
    Run
  | Flush
  | Finish

fromAction :: Action -> CInt
fromAction Run    = #{const BZ_RUN}
fromAction Flush  = #{const BZ_FLUSH}
fromAction Finish = #{const BZ_FINISH}

-- | The block size affects both the compression ratio achieved, and the amount
-- of memory needed for compression and decompression.
--
-- @'BlockSize' 1@ through @'BlockSize' 9@ specify the block size to be 100,000
-- bytes through 900,000 bytes respectively. The default is to use the maximum
-- block size.
--
-- Larger block sizes give rapidly diminishing marginal returns. Most of the
-- compression comes from the first two or three hundred k of block size, a
-- fact worth bearing in mind when using bzip2 on small machines. It is also
-- important to appreciate that the decompression memory requirement is set at
-- compression time by the choice of block size.
--
-- * In general, try and use the largest block size memory constraints allow,
-- since that maximises the compression achieved.
--
-- * Compression and decompression speed are virtually unaffected by block
-- size.
--
-- Another significant point applies to files which fit in a single block -
-- that means most files you'd encounter using a large block size. The amount
-- of real memory touched is proportional to the size of the file, since the
-- file is smaller than a block. For example, compressing a file 20,000 bytes
-- long with the flag @'BlockSize' 9@ will cause the compressor to allocate
-- around 7600k of memory, but only touch 400k + 20000 * 8 = 560 kbytes of it.
-- Similarly, the decompressor will allocate 3700k but only touch 100k + 20000
-- * 4 = 180 kbytes.
--
data BlockSize =
    DefaultBlockSize -- ^ The default block size is also the maximum.
  | BlockSize Int    -- ^ A specific block size between 1 and 9.

fromBlockSize :: BlockSize -> CInt
fromBlockSize DefaultBlockSize = 9
fromBlockSize (BlockSize n)
            | n >= 1 && n <= 9 = fromIntegral n
            | otherwise        = error "BlockSize must be in the range 1..9"

-- | For files compressed with the default 900k block size, decompression will
-- require about 3700k to decompress. To support decompression of any file in
-- less than 4Mb there is the option to decompress using approximately half
-- this amount of memory, about 2300k. Decompression speed is also halved,
-- so you should use this option only where necessary. 
--
data MemoryLevel =
    DefaultMemoryLevel -- ^ The default.
  | MinMemoryLevel     -- ^ Use minimum memory dusing decompression. This
                       --   halves the memory needed but also halves the
                       --   decompression speed.

fromMemoryLevel :: MemoryLevel -> CInt
fromMemoryLevel DefaultMemoryLevel = 0
fromMemoryLevel MinMemoryLevel     = 1

-- | The 'WorkFactor' parameter controls how the compression phase behaves when
-- presented with worst case, highly repetitive, input data. If compression
-- runs into difficulties caused by repetitive data, the library switches from
-- the standard sorting algorithm to a fallback algorithm. The fallback is
-- slower than the standard algorithm by perhaps a factor of three, but always
-- behaves reasonably, no matter how bad the input.
--
-- Lower values of 'WorkFactor' reduce the amount of effort the standard
-- algorithm will expend before resorting to the fallback. You should set this
-- parameter carefully; too low, and many inputs will be handled by the
-- fallback algorithm and so compress rather slowly, too high, and your
-- average-to-worst case compression times can become very large. The default
-- value of 30 gives reasonable behaviour over a wide range of circumstances.
--
-- * Note that the compressed output generated is the same regardless of
-- whether or not the fallback algorithm is used.
--
data WorkFactor =
    DefaultWorkFactor -- ^ The default work factor is 30.
  | WorkFactor Int    -- ^ Allowable values range from 1 to 250 inclusive.

fromWorkFactor :: WorkFactor -> CInt
fromWorkFactor DefaultWorkFactor = 0
fromWorkFactor (WorkFactor n)
      | n >= 1 && n <= 250 = fromIntegral n
      | otherwise          = error "WorkFactor must be in the range 1..250"

-- | The 'Verbosity' parameter is a number between 0 and 4. 0 is silent, and
-- greater numbers give increasingly verbose monitoring\/debugging output.
--
data Verbosity = Silent        -- ^ No output. This is the default.
               | Verbosity Int -- ^ A specific level between 0 and 4.

fromVerbosity :: Verbosity -> CInt
fromVerbosity Silent        = 0
fromVerbosity (Verbosity n)
         | n >= 0 && n <= 4 = fromIntegral n
         | otherwise        = error "Verbosity must be in the range 0..4"

withStreamPtr :: (Ptr StreamState -> IO a) -> Stream a
withStreamPtr f = do
  stream <- getStreamState
  unsafeLiftIO (withForeignPtr stream f)

withStreamState :: (StreamState -> IO a) -> Stream a
withStreamState f = do
  stream <- getStreamState
  unsafeLiftIO (withForeignPtr stream (f . StreamState))

setInAvail :: Int -> Stream ()
setInAvail val = withStreamPtr $ \ptr ->
  #{poke bz_stream, avail_in} ptr (fromIntegral val :: CUInt)

getInAvail :: Stream Int
getInAvail = liftM (fromIntegral :: CUInt -> Int) $
  withStreamPtr (#{peek bz_stream, avail_in})

setInNext :: Ptr Word8 -> Stream ()
setInNext val = withStreamPtr (\ptr -> #{poke bz_stream, next_in} ptr val)

getInNext :: Stream (Ptr Word8)
getInNext = withStreamPtr (#{peek bz_stream, next_in})

setOutFree :: Int -> Stream ()
setOutFree val = withStreamPtr $ \ptr ->
  #{poke bz_stream, avail_out} ptr (fromIntegral val :: CUInt)

getOutFree :: Stream Int
getOutFree = liftM (fromIntegral :: CUInt -> Int) $
  withStreamPtr (#{peek bz_stream, avail_out})

setOutNext  :: Ptr Word8 -> Stream ()
setOutNext val = withStreamPtr (\ptr -> #{poke bz_stream, next_out} ptr val)

getOutNext :: Stream (Ptr Word8)
getOutNext = withStreamPtr (#{peek bz_stream, next_out})

decompressInit :: Verbosity -> MemoryLevel -> Stream ()
decompressInit verbosity memoryLevel = do
  err <- withStreamState $ \bzstream ->
    bzDecompressInit bzstream
      (fromVerbosity verbosity)
      (fromMemoryLevel memoryLevel)
  failIfError err
  getStreamState >>= unsafeLiftIO . addForeignPtrFinalizer bzDecompressEnd

compressInit :: BlockSize -> Verbosity -> WorkFactor -> Stream ()
compressInit blockSize verbosity workFactor = do
  err <- withStreamState $ \bzstream ->
    bzCompressInit bzstream
      (fromBlockSize blockSize)
      (fromVerbosity verbosity)
      (fromWorkFactor workFactor)
  failIfError err
  getStreamState >>= unsafeLiftIO . addForeignPtrFinalizer bzCompressEnd

decompress_ :: Stream Status
decompress_ = do
  err <- withStreamState $ \bzstream ->
    bzDecompress bzstream
  failIfError err
  return (toStatus err)

compress_ :: Action -> Stream Status
compress_ action = do
  err <- withStreamState $ \bzstream ->
    bzCompress bzstream (fromAction action)
  failIfError err
  return (toStatus err)

-- | This never needs to be used as the stream's resources will be released
-- automatically when no longer needed, however this can be used to release
-- them early. Only use this when you can guarantee that the stream will no
-- longer be needed, for example if an error occurs or if the stream ends.
--
finalise :: Stream ()
finalise = getStreamState >>= unsafeLiftIO . finalizeForeignPtr

----------------------
-- The foreign imports

newtype StreamState = StreamState (Ptr StreamState)

foreign import ccall unsafe "bzlib.h BZ2_bzDecompressInit"
  bzDecompressInit :: StreamState -> CInt -> CInt -> IO CInt

foreign import ccall unsafe "bzlib.h BZ2_bzDecompress"
  bzDecompress :: StreamState -> IO CInt

foreign import ccall unsafe "bzlib.h &BZ2_bzDecompressEnd"
  bzDecompressEnd :: FinalizerPtr StreamState


foreign import ccall unsafe "bzlib.h BZ2_bzCompressInit"
  bzCompressInit :: StreamState -> CInt -> CInt -> CInt -> IO CInt

foreign import ccall unsafe "bzlib.h BZ2_bzCompress"
  bzCompress :: StreamState -> CInt -> IO CInt

foreign import ccall unsafe "bzlib.h &BZ2_bzCompressEnd"
  bzCompressEnd :: FinalizerPtr StreamState
