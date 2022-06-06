module Tests.Parser where

import Data.String.Interpolate
import Lola.Parser (ParserErrorBundle, expression)
import Relude
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, testCase, (@?), (@?=))
import Tests.Common (assertRegexMatch)
import Text.Megaparsec (errorBundlePretty, parse)

parseExpr :: Text -> Either ParserErrorBundle Text
parseExpr got = show <$> parse expression "" got

assertParse :: Text -> Text -> Assertion
assertParse got expected = parseExpr got @?= Right expected

assertParseError :: Text -> Text -> Assertion
assertParseError got pattern' = do
  let res = parseExpr got & first (toText . errorBundlePretty)
  isLeft res @? [i|test did not fail as expected: got `#{res}`|]
  fromLeft "" res `assertRegexMatch` pattern'

test_arithmetic :: TestTree
test_arithmetic =
  testGroup
    "Should parse arithmetic expressions"
    [ testCase "with priority" $
        "1+2 / 3- 4 *5" `assertParse` "(- (+ 1.0 (/ 2.0 3.0)) (* 4.0 5.0))",
      testCase "with parens" $
        "-(-1+2 / 3- 4 *5+ (6/ 7))" `assertParse` "(- (+ (- (+ (- 1.0) (/ 2.0 3.0)) (* 4.0 5.0)) (/ 6.0 7.0)))",
      testCase "with paren mismatch" $
        "-(-1+2 / 3- 4 *5+ (6/ 7)" `assertParseError` "unexpected end of input"
    ]