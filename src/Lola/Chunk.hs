module Lola.Chunk (Chunk (..), OpCode (..), Value) where

import Optics (makeFieldLabelsNoPrefix)
import Relude

data OpCode
  = OpConst
  | OpNeg
  | OpReturn

type Value = Double

data Chunk = Chunk
  { -- | A list of all bytecode bytes, followed by line numbers,
    code :: Seq (Int, Word8),
    -- | The constant pool.
    consts :: Seq Value
  }

makeFieldLabelsNoPrefix ''Chunk
