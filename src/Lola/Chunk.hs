module Lola.Chunk () where

import Relude

data Value
  = VBool Bool
  | VBoundMethod (IORef BoundMethod)
  | VClass (IORef Class)
  | VClosure (IORef Closure)
  | VFunc (IORef Func)
  | VInstance (IORef Instance)
  | VNativeFunc (NativeFunc)
  | VNil
  | VNum Double
  | VStr Text
