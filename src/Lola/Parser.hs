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
  | False'
  | Fun
  | For
  | If
  | Nil
  | Or
  | Print
  | Return
  | Super
  | This
  | True'
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

-- | Returns a 'Token' parser given a 'Text' parser to be mapped from and the expected 'TokenType'.
token :: TokenType -> Parser Text -> Parser Token
token tkType str = do
  pos <- getSourcePos
  str' <- str
  return $ Token tkType str' pos

-- | A reserved symbol in the Lox language.
rsym :: TokenType -> Text -> Parser Token
rsym tkType = token tkType . symbol

-- | A reserved keyword in the Lox language.
rword :: TokenType -> Text -> Parser Token
rword tkType str = token tkType rword'
  where
    rword' = lexeme . try $ string str <* notFollowedBy alphaNumChar

comma, dot, minus, plus, semicolon, slash, star, bang, bangEqual, equal, equalEqual, greater, greaterEqual, less, lessEqual :: Parser Token
comma = rsym Comma ","
dot = rsym Dot "."
minus = rsym Minus "-"
plus = rsym Plus "+"
semicolon = rsym Semicolon ";"
slash = rsym Slash "/"
star = rsym Star "*"
bang = rsym Bang "!"
bangEqual = rsym BangEqual "!="
equal = rsym Equal "="
equalEqual = rsym EqualEqual "=="
greater = rsym Greater ">"
greaterEqual = rsym GreaterEqual ">="
less = rsym Less "<"
lessEqual = rsym LessEqual "<="

and, break, class', continue, else', false, fun, for, if', nil, or, print, return', super, this, true, var, while :: Parser Token
and = rword And "and"
break = rword Break "break"
class' = rword Class "class"
continue = rword Continue "continue"
else' = rword Else "else"
false = rword False' "false"
fun = rword Fun "fun"
for = rword For "for"
if' = rword If "if"
nil = rword Nil "nil"
or = rword Or "or"
print = rword Print "print"
return' = rword Return "return"
super = rword Super "super"
this = rword This "this"
true = rword True' "true"
var = rword Var "var"
while = rword While "while"

identifier = undefined

strLit = undefined

numLit = undefined