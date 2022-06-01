module Lola.Parser () where

import Data.Bimap (Bimap)
import qualified Data.Bimap as Bimap
import qualified Data.Map.Strict as Map
import Data.String.Interpolate
import qualified Data.Text as T
import Data.Tuple.Optics
import Optics (makeFieldLabels)
import Optics.Operators
import Relude
import Text.Megaparsec (MonadParsec (notFollowedBy, try), Parsec, ParsecT, SourcePos (sourceColumn, sourceLine), choice, getSourcePos, manyTill, single, unPos)
import Text.Megaparsec.Char (alphaNumChar, char, letterChar, space1, string)
import qualified Text.Megaparsec.Char.Lexer as L
import qualified Prelude

kws :: Bimap TokenType Text
kws =
  fromList
    [ -- Regular keywords.
      (TBreak, "break"),
      (TClass, "class"),
      (TContinue, "continue"),
      (TElse, "else"),
      (TFalse, "false"),
      (TFun, "fun"),
      (TFor, "for"),
      (TIf, "if"),
      (TNil, "nil"),
      (TPrint, "print"),
      (TReturn, "return"),
      (TSuper, "super"),
      (TThis, "this"),
      (TTrue, "true"),
      (TVar, "var"),
      (TWhile, "while"),
      -- Keyword operators.
      (TAnd, "and"),
      (TOr, "or")
    ]

ops :: Bimap TokenType Text
ops =
  fromList
    [ (TComma, ","),
      (TDot, "."),
      (TMinus, "-"),
      (TPlus, "+"),
      (TSemicolon, ";"),
      (TSlash, "/"),
      (TStar, "*"),
      (TBang, "!"),
      (TBangEqual, "!="),
      (TEqual, "="),
      (TEqualEqual, "=="),
      (TGreater, ">"),
      (TGreaterEqual, ">="),
      (TLess, "<"),
      (TLessEqual, "<=")
    ]

data TokenType
  = TComma
  | TDot
  | TMinus
  | TPlus
  | TSemicolon
  | TSlash
  | TStar
  | TBang
  | TBangEqual
  | TEqual
  | TEqualEqual
  | TGreater
  | TGreaterEqual
  | TLess
  | TLessEqual
  | TIdent
  | TStrLit
  | TNumLit
  | TAnd
  | TBreak
  | TClass
  | TContinue
  | TElse
  | TFalse
  | TFun
  | TFor
  | TIf
  | TNil
  | TOr
  | TPrint
  | TReturn
  | TSuper
  | TThis
  | TTrue
  | TVar
  | TWhile
  deriving (Show, Ord, Eq)

data Token = Token
  { tokenType :: TokenType,
    tokenLexeme :: Text,
    tokenPos :: SourcePos
  }
  deriving (Eq)

instance Prelude.Show Token where
  show (Token ty str pos) = [i|`#{str}`::#{ty}@#{(ln, col)}|]
    where
      (ln, col) = (pos ^. #sourceLine & unPos, pos ^. #sourceColumn & unPos)

makeFieldLabels ''Token

type Parser :: Type -> Type
type Parser = Parsec Void Text

-- | A no-op parser which successes directly.
noop :: Parser ()
noop = return ()

space :: Parser ()
space =
  L.space
    space1
    (L.skipLineComment "//")
    (L.skipBlockCommentNested "/*" "*/")

symbol :: Text -> Parser Text
symbol = L.symbol space

lexeme :: Parser a -> Parser a
lexeme = L.lexeme space

-- | Returns a 'Token' parser given a 'Text' parser to be mapped from and the expected 'TokenType'.
toTokenParser :: TokenType -> Parser Text -> Parser Token
toTokenParser tkType str = do
  pos <- getSourcePos
  str' <- str
  return $ Token tkType str' pos

-- | A reserved keyword in the Lox language.
rword :: TokenType -> Text -> Parser Token
rword tkType str = toTokenParser tkType rword'
  where
    rword' = lexeme . try $ string str <* notFollowedBy alphaNumChar

-- | A reserved symbol in the Lox language.
rsym :: TokenType -> Text -> Parser Token
rsym tkType = toTokenParser tkType . symbol

kw :: Map TokenType (Parser Token)
kw = kws & Bimap.toMap & Map.mapWithKey rword

op :: Map TokenType (Parser Token)
op = kws & Bimap.toMap & Map.mapWithKey rsym

ident :: Parser Token
ident = toTokenParser TIdent ident'
  where
    ident' = lexeme . try $ identStr >>= check . toText
    identStr = (:) <$> letterChar <*> many (alphaNumChar <|> single '_')
    check str =
      if str `Bimap.memberR` kws
        then fail [i|keyword #{str} cannot be an identifier|]
        else return str

strLit :: Parser Token
strLit = toTokenParser TStrLit $ toText <$> (doubleQuote >> L.charLiteral `manyTill` doubleQuote)
  where
    doubleQuote = char '"'

floatLit, decimalLit, numLit :: Parser Token
floatLit = toTokenParser TNumLit $ show <$> L.signed noop L.float
decimalLit = toTokenParser TNumLit $ show <$> L.signed noop L.decimal
numLit = lexeme $ try floatLit <|> decimalLit

data Lit
  = LNil
  | LBool Bool
  | LNum Double
  | LStr Text
  deriving (Show)

data Expr
  = EAssign {assignName :: Token, assignVal :: Expr}
  | EBinary {lhs :: Expr, binOp :: Token, rhs :: Expr}
  | ECall {callee :: Expr, callArgs :: [Expr], callEnd :: Token}
  | EGet {getFrom :: Expr, getAttr :: Token}
  | EGrouping Expr
  | ELambda {lambdaParams :: [Token], lambdaBody :: [Stmt]}
  | ELiteral Lit
  | ELogical {lhs :: Expr, binOp :: Token, rhs :: Expr}
  | ESet {setName :: Expr, setAttr :: Token, setVal :: Expr}
  | ESuper {superKw :: Token, superMethod :: Token}
  | EThis Token
  | EUnary {unOp :: Token, rhs :: Expr}
  | EVariable Token
  deriving (Show)

data Stmt
  = SBlock [Stmt]
  | SClass {className :: Token, classSuper :: Maybe Expr, classMethods :: [Stmt]}
  | SExpr Expr
  | SFun {funName :: Token, funParams :: [Token], funBody :: [Stmt]}
  | SIf {ifCond :: Expr, ifThen :: Stmt, ifElse :: Maybe Stmt}
  | SJump Token
  | SPrint Expr
  | SReturn {returnKw :: Token, returnVal :: Maybe Expr}
  | SVarDecl {varName :: Token, varInit :: Maybe Expr}
  | SWhile {whileCond :: Expr, whileBody :: Stmt}
  deriving (Show)

-- TODO: Implement proper printing for the types above.

-- Grammar reference: https://craftinginterpreters.com/appendix-i.html

primary :: Parser Expr
primary =
  choice
    [ kw Map.! TTrue $> ELiteral (LBool True),
      kw Map.! TFalse $> ELiteral (LBool False),
      kw Map.! TNil $> ELiteral LNil,
      kw Map.! TThis <&> EThis
      -- TODO: Implement other cases.
    ]
