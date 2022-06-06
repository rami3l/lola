module Tests.Parser where

import Data.String.Interpolate
import Lola.Parser (ParserErrorBundle, expression)
import Relude
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertEqual, testCase, (@?), (@?=))
import Tests.Common (assertRegexMatch)
import Text.Megaparsec (errorBundlePretty, parse)

parseExpr :: Text -> Either Text Text
parseExpr got = parse expression "" got & bimap (toText . errorBundlePretty) show

assertParse :: Text -> Text -> Assertion
assertParse got expected = do
  let res = parseExpr got
  isRight res @? [i|test failed: got `#{fromLeft "" res}`|]
  fromRight "" res @?= expected

assertParseError :: Text -> Text -> Assertion
assertParseError got pattern' = do
  let res = parseExpr got
  isLeft res @? [i|test did not fail as expected: got `#{fromRight "" res}`|]
  fromLeft "" res `assertRegexMatch` pattern'

test_arithmetic :: TestTree
test_arithmetic =
  testGroup
    "Should parse arithmetic expressions"
    [ testCase "with precedence" $
        "1+2 / 3- 4 *5" `assertParse` "(- (+ 1.0 (/ 2.0 3.0)) (* 4.0 5.0))",
      testCase "with parens" $
        "-(-1+2 / 3- 4 *5+ (6/ 7))"
          `assertParse` "(- (+ (- (+ (- 1.0) (/ 2.0 3.0)) (* 4.0 5.0)) (/ 6.0 7.0)))",
      testCase "with paren mismatch" $
        -- TODO: Improve error message quality.
        "-(-1+2 / 3- 4 *5+ (6/ 7)" `assertParseError` "unexpected end of input",
      -- TODO: Add parser syncing?
      -- TODO: paren_mismatch_sync
      testCase "with binary misused as unary" $
        -- TODO: Improve error message quality.
        "*1" `assertParseError` [i|unexpected "\\*1"|],
      -- TODO: mul_used_as_unary_sync
      testCase "with assignments" $
        "a = b = c = 3" `assertParse` "(assign! a (assign! b (assign! c 3.0)))"
    ]

test_boolean :: TestTree
test_boolean =
  testGroup
    "Should parse booleans"
    [ testCase "with an inequality" $
        "-(-1+2) >=3- 4 *5+ (6/ 7)"
          `assertParse` "(>= (- (+ (- 1.0) 2.0)) (+ (- 3.0 (* 4.0 5.0)) (/ 6.0 7.0)))",
      -- TODO: inequality_used_as_unary
      -- TODO: inequality_used_as_unary_sync
      testCase "with `and`, `or` and `!`" $
        "foo == nil or !!bar and a != (b = c = 3)"
          `assertParse` "(or (== foo nil) (and (! (! bar)) (!= a (assign! b (assign! c 3.0)))))"
    ]

test_call :: TestTree
test_call =
  testGroup
    "Should parse function calls and get/set expressions"
    [ testCase "with complex calls" $
        "func (c) (u, r) (r(y), i) (n) (g) ()"
          `assertParse` "((((((func c) u r) (r y) i) n) g))",
      -- TODO: fun_call_typo
      testCase "with gets and sets" $
        "breakfast.omelette.filling.meat = ham"
          `assertParse` "(.set! (. (. breakfast omelette) filling) meat ham)",
      testCase "with chained method calls" $
        "egg.scramble(3).with(cheddar)"
          `assertParse` "((. ((. egg scramble) 3.0) with) cheddar)",
      testCase "with nested method calls" $
        "he.breakfast(omelette.filledWith(cheese), sausage)"
          `assertParse` "((. he breakfast) ((. omelette filledWith) cheese) sausage)",
      testCase "with `super`" $ "super.method()" `assertParse` "((. (super) method))"
    ]

-- TODO: Support lambda expressions.