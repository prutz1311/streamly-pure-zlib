{-# LANGUAGE ScopedTypeVariables #-}

module Codec.Compression.Zlib.Streamly (decompressZlib) where

import qualified Data.ByteString as B
import           Control.Monad.Primitive (PrimMonad(..), stToPrim)
import           Control.Monad.Catch (MonadThrow(..))
import qualified Streamly.Internal.Data.Pipe as SP
import qualified Codec.Compression.Zlib as PZ


-- We are using `m (PZ.ZlibDecoder (PrimState m))`
-- both as the consume state and the produce state
-- of the pipe
consume :: forall m. (MonadThrow m, PrimMonad m)
        => B.ByteString
        -> PZ.ZlibDecoder (PrimState m)
        -> m (SP.Step
                (SP.PipeState
                   (m (PZ.ZlibDecoder (PrimState m)))
                   (m (PZ.ZlibDecoder (PrimState m))))
                B.ByteString)
consume input (PZ.NeedMore f) =
  return $ SP.Continue $ SP.Produce $ stToPrim $ f input
consume _     (PZ.Chunk output act) =
  return $ SP.Continue $ SP.Produce $ return (PZ.Chunk output act)
consume _     PZ.Done =
  throwM $ PZ.DecompressionError "Finished with data remaining."
consume _     (PZ.DecompError e) = throwM e

produce :: forall m. (MonadThrow m, PrimMonad m)
        => PZ.ZlibDecoder (PrimState m)
        -> m (SP.Step
                (SP.PipeState
                   (m (PZ.ZlibDecoder (PrimState m)))
                   (m (PZ.ZlibDecoder (PrimState m))))
                B.ByteString)
produce (PZ.NeedMore f) =
  return $ SP.Continue $ SP.Consume $ return $ PZ.NeedMore f
produce (PZ.Chunk output act) =
  return $ SP.Yield output $ SP.Produce $ stToPrim act
produce PZ.Done =
  return $ SP.Continue $ SP.Consume $ return PZ.Done
produce (PZ.DecompError e) = throwM e

decompressZlib :: forall m. (MonadThrow m, PrimMonad m)
               => SP.Pipe m B.ByteString B.ByteString
decompressZlib = 
  SP.Pipe 
    (\act chunk -> act >>= consume chunk)
    (\act -> act >>= produce)
    (stToPrim PZ.decompressIncremental)
