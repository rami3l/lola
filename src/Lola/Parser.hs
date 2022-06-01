module Lola.Parser () where

import Data.Bimap (Bimap)
import qualified Data.Bimap as Bimap
import qualified Data.Map.Strict as Map
import Data.String.Interpolate
import qualified Data.Text as T
import Optics (makeFieldLabels)
import Optics.Operators
import Relude
import Text.Megaparsec (MonadParsec (notFollowedBy, try), Parsec, ParsecT, SourcePos, getSourcePos, manyTill, single)
import Text.Megaparsec.Char (alphaNumChar, char, letterChar, space1, string)
import qualified Text.Megaparsec.Char.Lexer as L

kws :: Bimap TokenType Text
kws =
  fromList
    [ -- Regular keywords.
      (Break, "break"),
      (Class, "class"),
      (Continue, "continue"),
      (Else, "else"),
      (False', "false"),
      (Fun, "fun"),
      (For, "for"),
      (If, "if"),
      (Nil, "nil"),
      (Print, "print"),
      (Return, "return"),
      (Super, "super"),
      (This, "this"),
      (True', "true"),
      (Var, "var"),
      (While, "while"),
      -- Keyword operators.
      (And, "and"),
      (Or, "or")
    ]

ops :: Bimap TokenType Text
ops =
  fromList
    [ (Comma, ","),
      (Dot, "."),
      (Minus, "-"),
      (Plus, "+"),
      (Semicolon, ";"),
      (Slash, "/"),
      (Star, "*"),
      (Bang, "!"),
      (BangEqual, "!="),
      (Equal, "="),
      (EqualEqual, "=="),
      (Greater, ">"),
      (GreaterEqual, ">="),
      (Less, "<"),
      (LessEqual, "<=")
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
  | Ident
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
  deriving (Show, Ord, Eq)

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

kwParsers :: Map TokenType (Parser Token)
kwParsers = kws & Bimap.toMap & Map.mapWithKey rword

opParsers :: Map TokenType (Parser Token)
opParsers = ops & Bimap.toMap & Map.mapWithKey rsym

ident :: Parser Token
ident = token Ident ident'
  where
    ident' = lexeme . try $ identStr >>= check . toText
    identStr = (:) <$> letterChar <*> many (alphaNumChar <|> single '_')
    check str =
      if str `Bimap.memberR` kws
        then fail [i|keyword #{str} cannot be an identifier|]
        else return str

strLit :: Parser Token
strLit = token StrLit $ toText <$> (doubleQuote >> L.charLiteral `manyTill` doubleQuote)
  where
    doubleQuote = char '"'

numLit :: Parser Token
numLit = token NumLit $ show <$> L.signed space L.float