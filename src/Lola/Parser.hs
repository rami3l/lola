module Lola.Parser
  ( Expr,
    Parser,
    ParserErrorBundle,
    Stmt,
    Token,
    TokenType,
    expression,
  )
where

import Data.Bimap (Bimap)
import qualified Data.Bimap as Bimap
import qualified Data.Map.Strict as Map
import Data.String.Interpolate
import Optics (makeFieldLabels)
import Relude
import Text.Megaparsec
  ( MonadParsec (notFollowedBy, try),
    ParseErrorBundle,
    Parsec,
    SourcePos,
    between,
    choice,
    getSourcePos,
    manyTill,
    option,
    sepBy,
    single,
  )
import Text.Megaparsec.Char
  ( alphaNumChar,
    char,
    letterChar,
    space1,
    string,
  )
import qualified Text.Megaparsec.Char.Lexer as L
import Prelude (read)
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
    [ (TLParen, "("),
      (TRParen, ")"),
      (TLBrace, "{"),
      (TRBrace, "}"),
      (TComma, ","),
      (TDot, "."),
      (TMinus, "-"),
      (TPlus, "+"),
      (TSemicolon, ";"),
      (TSlash, "/"),
      (TStar, "*"),
      (TBang, "!"),
      (TBangEqual, "!="),
      (TEqualEqual, "=="),
      (TEqual, "="),
      (TGreaterEqual, ">="),
      (TGreater, ">"),
      (TLessEqual, "<="),
      (TLess, "<")
    ]

data TokenType
  = TLParen
  | TRParen
  | TLBrace
  | TRBrace
  | TComma
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

makeFieldLabels ''Token

instance Prelude.Show Token where show = toString . tokenLexeme

type ParserError = Void

type ParserStream = Text

type Parser :: Type -> Type
type Parser = Parsec ParserError ParserStream

type ParserErrorBundle = ParseErrorBundle ParserStream ParserError

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

kwMap, opMap :: Map TokenType (Parser Token)
kwMap = kws & Bimap.toMap & Map.mapWithKey rword
opMap = ops & Bimap.toMap & Map.mapWithKey rsym

kw, op :: TokenType -> Parser Token
kw = (kwMap Map.!)
op = (opMap Map.!)

ident :: Parser Token
ident = toTokenParser TIdent ident'
  where
    ident' = lexeme . try $ check . toText =<< identStr
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
floatLit = toTokenParser TNumLit $ show @_ @Double <$> L.float
decimalLit = toTokenParser TNumLit $ show @_ @Integer <$> L.decimal
numLit = lexeme $ try floatLit <|> decimalLit

data Lit
  = LNil
  | LBool Bool
  | LNum Double
  | LStr Text

instance Prelude.Show Lit where
  show LNil = "nil"
  show (LBool b) = show b
  show (LNum n) = show n
  show (LStr s) = show s

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
    [ kw TTrue $> ELiteral (LBool True),
      kw TFalse $> ELiteral (LBool False),
      kw TNil $> ELiteral LNil,
      kw TThis <&> EThis,
      numLit <&> ELiteral . LNum . read . toString . tokenLexeme,
      strLit <&> ELiteral . LStr . tokenLexeme,
      ident <&> EVariable,
      expression & between (op TLParen) (op TRParen) <&> EGrouping,
      ESuper <$> kw TSuper <*> (op TDot *> ident)
    ]

toBinParser :: Parser Expr -> (Expr -> Parser Expr) -> Parser Expr
toBinParser car cdr = do c <- car; go c & option c
  where
    go c = do c' <- cdr c; go c' & option c'

call :: Parser Expr
call = toBinParser primary \c -> goArgs c <|> goGet c
  where
    goArgs c = ECall c <$> (op TLParen *> args) <*> op TRParen
    goGet c = op TDot *> ident <&> EGet c
    args = expression `sepBy` op TComma

unary, factor, term, comparison, equality, logicAnd, logicOr :: Parser Expr
unary = (EUnary <$> (op TBang <|> op TMinus) <*> unary) <|> call
factor = toBinParser unary \c ->
  EBinary c <$> (op TSlash <|> op TStar) <*> unary
term = toBinParser factor \c ->
  EBinary c <$> (op TMinus <|> op TPlus) <*> factor
comparison = toBinParser term \c ->
  EBinary c
    <$> (op TGreaterEqual <|> op TGreater <|> op TLessEqual <|> op TLess)
    <*> term
equality = toBinParser comparison \c ->
  EBinary c <$> (op TBangEqual <|> op TEqualEqual) <*> comparison
logicAnd = toBinParser equality \c -> EBinary c <$> kw TAnd <*> equality
logicOr = toBinParser logicAnd \c -> EBinary c <$> kw TOr <*> logicAnd

expression :: Parser Expr
expression = do
  lhs' <- logicOr
  option lhs' do
    rhs' <- op TEqual *> expression -- Assignment expression detected.
    case lhs' of
      EVariable name -> return $ EAssign name rhs'
      EGet obj name -> return $ ESet obj name rhs'
      _ -> fail "Error while parsing an Assignment Expression: can only assign to a variable"

instance Prelude.Show Expr where
  show (EAssign name val) = [i|(assign! #{name} #{val})|]
  show (EBinary lhs op' rhs) = [i|(#{op'} #{lhs} #{rhs})|]
  show (ECall callee args _)
    | null args = [i|(#{callee})|]
    | otherwise = [i|(#{callee} #{intercalateS args})|]
  show (EGet obj name) = [i|(. #{obj} #{name})|]
  show (EGrouping inner) = show inner
  show (ELambda params body) =
    let body' = if null body then "'()" else intercalateS body
     in [i|(λ (#{intercalateS params}) #{body'})|]
  show (ELiteral lit) = show lit
  show (ELogical lhs op' rhs) = [i|(#{op'} #{lhs} #{rhs})|]
  show (ESet obj name to) = [i|(.set! #{obj} #{name} #{to})|]
  show (ESuper _ method) = [i|(. (super) #{method})|]
  show (EThis _) = "(this)"
  show (EUnary op' rhs) = [i|(#{op'} #{rhs})|]
  show (EVariable var) = var & tokenLexeme & toString

intercalateS :: Show a => [a] -> String
intercalateS = intercalate " " . fmap show

showLisp :: Show a => [a] -> String
showLisp [] = "'()"
showLisp ts = "(" <> intercalateS ts <> ")"