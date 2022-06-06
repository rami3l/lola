module Tests.Common where

import Data.String.Interpolate
import Relude
import Test.Tasty.HUnit (Assertion, (@?))
import Text.Regex (matchRegex, mkRegex)

assertRegexMatch :: Text -> Text -> Assertion
assertRegexMatch got pattern' =
  (mkRegex (toString pattern') `matchRegex` toString got & isJust)
    @? [i|regex mismatch: expected `#{pattern'}`, got `#{got}`|]