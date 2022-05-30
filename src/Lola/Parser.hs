module Lola.Parser where

import Optics.Operators
import Relude
import Text.Parsec (SourcePos, getPosition)
import Text.Parsec.Language (emptyDef, javaStyle)
import Text.Parsec.String (Parser)
import Text.Parsec.Token (GenLanguageDef)
import qualified Text.Parsec.Token as Token

lexer :: Token.TokenParser ()
lexer =
  Token.makeTokenParser
    javaStyle
      { Token.reservedOpNames = [".", ",", "+", "*", "-", "/", "!", "!=", "=", "==", ">", ">=", "<", "<="] ++ kwOps,
        Token.reservedNames = ["break", "class", "continue", "else", "false", "fun", "for", "if", "nil", "print", "return", "super", "this", "true", "var", "while"] ++ kwOps,
        Token.caseSensitive = Relude.True
      }
  where
    kwOps = ["and", "or"]

data Token = Token
  { tokenType :: TokenType,
    lexeme :: String,
    pos :: SourcePos
  }
  deriving (Show)

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
  | SingleLineComment
  deriving (Show)

reservedOp :: String -> Parser Token
reservedOp str = do
  pos <- getPosition
  lexer `Token.reservedOp` str
  return $ Token Comma str pos

comma, dot, minus, plus :: Parser Token
comma = reservedOp ","
dot = reservedOp ","
minus = reservedOp "-"
plus = reservedOp "+"