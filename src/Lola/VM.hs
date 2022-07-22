module Lola.VM () where

import Data.Sequence qualified as Sequence
import Lola.Chunk
import Optics (Field2 (_2), Ixed (ix), makeFieldLabelsNoPrefix, (^.))
import Relude

data InterpretError = CompileError !Text | RuntimeError !Text

-- The Lox Virtual Machine.
data VM = VM
  { chunk :: IORef Chunk,
    ip :: Word,
    stack :: Seq Value
  }

makeFieldLabelsNoPrefix ''VM

vm :: IORef Chunk -> IO VM
vm chunkRef = do
  chunk <- chunkRef & readIORef
  pure
    VM
      { chunk = chunkRef,
        ip = chunk ^. #code ^. (ix 0) ^. _2,
        stack = Sequence.empty
      }
