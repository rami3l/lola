module Lola.VM () where

import Relude

-- The Lox Virtual Machine.
data VM = VM
  { frames :: Seq CallFrame,
    stack :: Seq Value,
    globals :: Table,
    strings :: Table,
    openUpvalues :: Seq (IORef Upvalue),
    initString :: IORef String
  }

data Closure = Closure -- TODO: FINISH THIS

data CallFrame = CallFrame
  { -- | The active function call.
    closure :: IORef Closure,
    -- | The Program Counter.
    pc :: Int,
    -- | The first VM stack slot that this function can use.
    slot :: Int
  }

callFrame :: IORef Closure -> Int -> CallFrame
callFrame closure slot = CallFrame closure 0 slot
