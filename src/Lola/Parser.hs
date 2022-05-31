module Lola.Parser () where

import Optics (makeFieldLabels)
import Optics.Operators
import Relude
import Text.Megaparsec (MonadParsec (notFollowedBy, try), Parsec, ParsecT, SourcePos, getSourcePos)
import Text.Megaparsec.Char (alphaNumChar, space1, string)
import qualified Text.Megaparsec.Char.Lexer as L

keywords :: [Text] -- list of reserved words
keywords =
  [ -- Regular keywords.
    "break",
    "class",
    "continue",
    "else",
    "false",
    "fun",
    "for",
    "if",
    "nil",
    "print",
    "return",
    "super",
    "this",
    "true",
    "var",
    "while",
    -- Keyword operators.
    "and",
    "or"
  ]

data TokenType
  = Comma
  | Dot
  | Minus
  | Plus
  | Semicolon
  | Slash
  | Star
  | Bang
  | BangEqual
  | Equal
  | EqualEqual
  | Greater
  | GreaterEqual
  | Less
  | LessEqual
  | Identifier
  | StrLit
  | NumLit
  | And
  | Break
  | Class
  | Continue
  | Else
  | False
  | Fun
  | For
  | If
  | Nil
  | Or
  | Print
  | Return
  | Super
  | This
  | True
  | Var
  | While
  deriving (Show)

data Token = Token
  { tokenType :: TokenType,
    tokenLexeme :: Text,
    tokenPos :: SourcePos
  }
  deriving (Show)

makeFieldLabels ''Token

type Parser :: Type -> Type
type Parser = Parsec Void Text

space :: Parser ()
space =
  L.space
    space1
    (L.skipLineComment "//")
    (L.skipBlockCommentNested "/*" "*/")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme space

symbol :: Text -> Parser Text
symbol = L.symbol space

-- | A reserved symbol in the Lox language.
rsym :: TokenType -> Text -> Parser Token
rsym tkType str = do
  pos <- getSourcePos
  str' <- symbol str
  return $ Token tkType str' pos

-- | A reserved keyword in the Lox language.
rword :: TokenType -> Text -> Parser Token
rword tkType str = do
  pos <- getSourcePos
  str' <- lexeme . try $ string str <* notFollowedBy alphaNumChar
  return $ Token tkType str' pos

-- TODO: Refactor the two functions above.

comma, dot, minus, plus :: Parser Token
comma = rsym Comma ","
dot = rsym Dot "."
minus = rsym Minus "-"
plus = rsym Plus "+"