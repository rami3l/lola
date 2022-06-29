module Lola
  ( someFunc,
  )
where

import Language.C.Inline qualified as C
import Relude

C.include "Lola/vm.h"

someFunc :: IO ()
someFunc = [C.exp| void { greet() } |]
