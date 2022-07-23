module Lola.VM () where

import Control.Exception (mapException)
import Data.Maybe (fromJust)
import Data.Sequence qualified as Sequence
import Lola.Chunk (Chunk (..), Value)
import Optics (makeFieldLabelsNoPrefix, (%!~))
import Relude
import UnliftIO (MonadUnliftIO, mapExceptionM, onException, throwIO)

data CompileError = CompileError !Text

data RuntimeException = RuntimeException !Text deriving (Show, Typeable)

instance Exception RuntimeException

-- The Lox Virtual Machine.
data VM = VM
  { chunk :: IORef Chunk,
    ip :: Int,
    stack :: Seq Value
  }

makeFieldLabelsNoPrefix ''VM

vm :: MonadUnliftIO m => IORef Chunk -> m (IORef VM)
vm chunk = newIORef VM {chunk, ip = 0, stack = Sequence.empty}

execVM :: MonadUnliftIO m => IORef VM -> m ()
execVM vmRef = do
  VM {chunk = chunkRef, ip} <- vmRef & readIORef
  Chunk {code} <- chunkRef & readIORef
  (ln, byte) <-
    (pure $ code Sequence.!? ip & fromJust)
      & onException (throwIO $ RuntimeException "ip out of range")
  -- Increment @ip@.
  vmRef `modifyIORef'` (#ip %!~ succ)
  -- TODO
  undefined
