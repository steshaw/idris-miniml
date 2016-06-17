module Lexer

import Lightyear
import Lightyear.Char
import Lightyear.Strings

public export
data Lexeme
  = INT Integer
  | TINT
  | TBOOL
  | TRUE
  | FALSE
  | FUN
  | IS
  | IF
  | THEN
  | ELSE
  | LET
  | SEMICOLON2
  | EQUAL
  | LESS
  | TARROW
  | COLON
  | LPAREN
  | RPAREN
  | PLUS
  | MINUS
  | TIMES
  | VAR String

-- XXX: deriving Eq?
export
implementation Eq Lexeme where
  (INT i1) == (INT i2) = i1 == i2
  TINT == TINT = True
  TBOOL == TBOOL = True
  TRUE == TRUE = True
  FALSE == FALSE = True
  FUN == FUN = True
  IS == IS = True
  IF == IF = True
  THEN == THEN = True
  ELSE == ELSE = True
  LET == LET = True
  SEMICOLON2 == SEMICOLON2 = True
  EQUAL == EQUAL = True
  LESS == LESS = True
  TARROW == TARROW = True
  COLON == COLON = True
  LPAREN == LPAREN = True
  RPAREN == RPAREN = True
  PLUS == PLUS = True
  MINUS == MINUS = True
  TIMES == TIMES = True
  (VAR nm1) == (VAR nm2) = nm1 == nm2
  _ == _ = False

-- XXX: deriving Show?
export
implementation Show Lexeme where
  show _ = "<lexeme>"

getInteger : List (Fin 10) -> Integer
getInteger = foldl (\a => \b => 10 * a + cast b) 0

int : Parser Lexeme
int = do
  digits <- some digit
  pure $ INT (getInteger digits)

kw : String -> Lexeme
kw "int" = TINT
kw "bool" = TBOOL
kw "true" = TRUE
kw "false" = FALSE
kw "fun" = FUN
kw "is" = IS
kw "if" = IF
kw "then" = THEN
kw "else" = ELSE
kw "let" = LET
kw nm = VAR nm

varKw : Parser Lexeme
varKw = do
  nm <- some letter
  pure $ kw (pack nm)

tok : Parser Lexeme
tok = int
  <|> varKw
  <|> (string ";;" *> pure SEMICOLON2)
  <|> (string "=" *> pure EQUAL)
  <|> (string "<" *> pure LESS)
  <|> (string "->" *> pure TARROW)
  <|> (string ":" *> pure COLON)
  <|> (string "(" *> pure LPAREN)
  <|> (string ")" *> pure RPAREN)
  <|> (string "+" *> pure PLUS)
  <|> (string "-" *> pure MINUS)
  <|> (string "×" *> pure TIMES)
  <|> (string "*" *> pure TIMES)

export
lexer : Parser (List Lexeme)
lexer = spaces *> (many (tok <* spaces))
