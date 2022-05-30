module Lola (
  someFunc,
) where

import Data.String.Interpolate
import Optics (makeFieldLabels)
import Optics.Operators
import Relude

data SomeStruct = SomeStruct
  { foo :: Int
  , bar :: String
  }
  deriving (Show, Generic)
makeFieldLabels ''SomeStruct

someFunc :: IO ()
someFunc = putStrLn [i|Hello, #{struct ^. #foo} #{struct ^. #bar}!|]
 where
  struct = SomeStruct 42 "fishes"
