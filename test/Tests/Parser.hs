module Tests.Parser where

import Data.String.Interpolate
import Lola.Parser (Parser, expression, program)
import Relude
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, testCase, (@?=))
import Tests.Common (assertRegexMatch)
import Text.Megaparsec (errorBundlePretty, parse)

parse' :: Show a => Parser a -> Text -> Either Text Text
parse' parser got = parse parser "" got & bimap (toText . errorBundlePretty) show

assert' :: (Show a, Eq a) => Either Text a -> a -> Assertion
assert' (Right got) expected = got @?= expected
assert' (Left e) _ = error [i|test failed: got `#{e}`|]

assertError :: Show a => Either Text a -> Text -> Assertion
assertError (Right a) _ = putStrLn $ "test did not fail as expected: got `" <> show a <> "`"
assertError (Left got) pattern' = got `assertRegexMatch` pattern'

-- Expressions:

assertExpr, assertExprError :: Text -> Text -> Assertion
assertExpr = assert' . parse' expression
assertExprError = assertError . parse' expression

test_arithmetic :: TestTree
test_arithmetic =
  testGroup
    "Should parse arithmetic expressions"
    [ testCase "with precedence" $
        "1+2 / 3- 4 *5" `assertExpr` "(- (+ 1 (/ 2 3)) (* 4 5))",
      testCase "with parens" $
        "-(-1+2 / 3- 4 *5+ (6/ 7))"
          `assertExpr` "(- (+ (- (+ (- 1) (/ 2 3)) (* 4 5)) (/ 6 7)))",
      testCase "with paren mismatch" $
        -- TODO: Improve error message quality.
        "-(-1+2 / 3- 4 *5+ (6/ 7)" `assertExprError` "unexpected end of input",
      -- TODO: Add parser syncing?
      -- TODO: paren_mismatch_sync
      testCase "with binary misused as unary" $
        -- TODO: Improve error message quality.
        "*1" `assertExprError` [i|unexpected "\\*1"|],
      -- TODO: mul_used_as_unary_sync
      testCase "with assignments" $
        "a = b = c = 3" `assertExpr` "(assign! a (assign! b (assign! c 3)))"
    ]

test_boolean :: TestTree
test_boolean =
  testGroup
    "Should parse booleans"
    [ testCase "with an inequality" $
        "-(-1+2) >=3- 4 *5+ (6/ 7)"
          `assertExpr` "(>= (- (+ (- 1) 2)) (+ (- 3 (* 4 5)) (/ 6 7)))",
      -- TODO: inequality_used_as_unary
      -- TODO: inequality_used_as_unary_sync
      testCase "with `and`, `or` and `!`" $
        "foo == nil or !!bar and a != (b = c = 3)"
          `assertExpr` "(or (== foo nil) (and (! (! bar)) (!= a (assign! b (assign! c 3)))))"
    ]

test_call :: TestTree
test_call =
  testGroup
    "Should parse function calls and get/set expressions"
    [ testCase "with complex calls" $
        "func (c) (u, r) (r(y), i) (n) (g) ()"
          `assertExpr` "((((((func c) u r) (r y) i) n) g))",
      -- TODO: fun_call_typo
      testCase "with gets and sets" $
        "breakfast.omelette.filling.meat = ham"
          `assertExpr` "(.set! (. (. breakfast omelette) filling) meat ham)",
      testCase "with chained method calls" $
        "egg.scramble(3).with(cheddar)"
          `assertExpr` "((. ((. egg scramble) 3) with) cheddar)",
      testCase "with nested method calls" $
        "he.breakfast(omelette.filledWith(cheese), sausage)"
          `assertExpr` "((. he breakfast) ((. omelette filledWith) cheese) sausage)",
      testCase "with `super`" $ "super.method()" `assertExpr` "((. (super) method))"
    ]

test_lambda :: TestTree
test_lambda =
  testGroup
    "Should handle lambda expressions"
    [ testCase "with no-op" $ "fun () { }" `assertExpr` "(lambda () '())",
      testCase "with in-place call" $
        "fun () { } ()"
          `assertExpr` "((lambda () '()))",
      testCase "with params and body" $
        "fun (a, b, c, d) { print a * b - c / d; }"
          `assertExpr` "(lambda (a b c d) (print (- (* a b) (/ c d))))"
    ]

-- Statements (and Declarations):

assertProg :: Text -> [Text] -> Assertion
assertProg = assert' . bimap (toText . errorBundlePretty) (show <$>) . parse program ""

assertProgError :: Text -> Text -> Assertion
assertProgError = assertError . parse' program

test_simple :: TestTree
test_simple =
  testGroup
    "Should handle simple statements"
    [ testCase "with print" $
        "print -(-1+2) >=3;"
          `assertProg` ["(print (>= (- (+ (- 1) 2)) 3))"],
      testCase "with `if`-`else`" $
        "var year; if (2 + 2 == 5) year = 1984; else year = 2022;"
          `assertProg` [ "(var year)",
                         "(if (== (+ 2 2) 5) (assign! year 1984) (assign! year 2022))"
                       ],
      -- TODO: if_stmt_no_then
      testCase "with `if`" $
        "var year; if (2 + 2 == 5) year = 1984;"
          `assertProg` [ "(var year)",
                         "(if (== (+ 2 2) 5) (assign! year 1984))"
                       ],
      testCase "with `if`, nested & unbraced" $
        [__i|
          if (first)
            if (second) whenTrue;
            else whenFalse;
        |]
          `assertProg` ["(if first (if second whenTrue whenFalse))"],
      testCase "with `while`" $
        "while (i <= 5) { product = product * i; i = i + 1; }"
          `assertProg` ["(while (<= i 5) (begin (assign! product (* product i)) (assign! i (+ i 1))))"],
      testCase "with `for`" $
        "for (i = product = 1; i <= 5; i = i + 1) { product = product * i; }"
          `assertProg` ["(begin (assign! i (assign! product 1)) (while (<= i 5) (begin (begin (assign! product (* product i))) (assign! i (+ i 1)))))"],
      testCase "with `for`, pure loop" $
        "for (;;) { product = product * i; }"
          `assertProg` ["(begin (while true (begin (begin (assign! product (* product i))))))"]
    ]