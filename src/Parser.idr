module Parser

import Lightyear
import Control.Monad.Identity
import Lexer
import Syntax

public export
Parser : Type -> Type
Parser = ParserT (List Lexeme) Identity

is : (Monad m, Stream tok str, Eq tok)
     => tok
     -> ParserT str m tok
is l = satisfy (== l)

implementation Stream Lexeme (List Lexeme) where
  uncons []        = Nothing
  uncons (x :: xs) = Just (x, xs)

E : Type
E = Expr

infixOp : Lexeme -> (a -> a -> a) -> Parser (a -> a -> a)
infixOp l ctor = do
  _ <- satisfy (== l)
  pure ctor

addOp : Parser (E -> E -> E)
addOp = infixOp PLUS Plus <|> infixOp MINUS Minus

mulOp : Parser (E -> E -> E)
mulOp = infixOp TIMES Times
  
boolOp : Parser (E -> E -> E)
boolOp = infixOp EQUAL Equal <|> infixOp LESS Less

tyOp : Parser (Ty -> Ty -> Ty)
tyOp = infixOp TARROW TArrow

-- Actually no "operator" here. Function application is just juxtaposition.
applyOp : Parser (E -> E -> E)
applyOp = pure Apply

isInt : Lexeme -> Bool
isInt (INT n) = True
isInt _       = False

int : Parser Expr
int = do
  (INT i) <- satisfy isInt
  pure (EInt i)

neg : Parser Expr
neg = do
  _ <- is MINUS
  (EInt i) <- int
  pure $ EInt (-i)

lparen : Parser Lexeme
lparen = is LPAREN

rparen : Parser Lexeme
rparen = is RPAREN

semicolon2 : Parser Lexeme
semicolon2 = is SEMICOLON2

true : Parser Expr
true = is TRUE *> pure (EBool True)
  
false : Parser Expr
false = is FALSE *> pure (EBool False)

isVar : Lexeme -> Bool
isVar (VAR _) = True
isVar _         = False

nm : Parser Name
nm = do
  (VAR nm) <- satisfy isVar
  pure $ nm

var : Parser Expr
var = do
  name <- nm
  pure $ Var name

mutual
  def : Parser TopLevelCmd
  def = do
    _ <- is LET
    name <- nm
    _ <- is EQUAL
    e <- expr
    pure $ TopDef name e

  expr : Parser Expr
  expr = boolean

  boolean : Parser Expr
  boolean = arith `chainl1` boolOp

  arith : Parser Expr
  arith = term `chainl1` addOp

  term : Parser Expr
  term = factor `chainl1` mulOp
 
  ifExpr : Parser Expr
  ifExpr = do
    _ <- is IF
    c <- expr
    _ <- is THEN
    t <- expr
    _ <- is ELSE
    f <- expr
    pure $ If c t f

  ty : Parser Ty
  ty = tyFactor `chainr1` tyOp

  tyFactor : Parser Ty
  tyFactor = (is TBOOL *> pure TBool)
         <|> (is TINT  *> pure TInt)
         <|> (lparen *>| ty <*| rparen)

  fun : Parser Expr
  fun = do
    _ <- is FUN
    funName <- nm
    _ <- lparen
    parName <- nm
    _ <- is COLON
    parTy <- ty
    _ <- rparen
    _ <- is COLON
    resTy <- ty
    _ <- is IS
    body <- expr
    pure $ Fun funName parName parTy resTy body

  factor : Parser Expr
  factor = (nonApp `chainl1` applyOp)
       <|> ifExpr
       <|> fun

  nonApp : Parser Expr
  nonApp = var
       <|> true
       <|> false
       <|> int
       <|> (lparen *>| expr <*| rparen)

export
toplevel : Parser (List TopLevelCmd)
toplevel = do
  cmd <- TopExpr <$> expr <|> def
  esOpt <- opt (semicolon2 *> toplevel)
  _ <- opt semicolon2
  pure $ cmd :: case esOpt of
    Just es => es
    None    => []

{-
toplevel ::=
  | EOF
  | def EOF
  | def ";;" EOF
  | expr EOF
  | expr ";; EOF
  | def ";;" toplevel
  | expr ";; toplevel

def ::= LET VAR EQUAL expr

expr ::=
  | non_app
  | app
  | arith
  | boolean
  | IF expr THEN expr ELSE expr
  | FUN VAR "(" VAR ":" ty ")" ":" ty IS expr

app ::=
  | app non_app -- huh?
  | non_app non_app

non_app ::=
  | VAR
  | TRUE
  | FALSE
  | INT
  | "(" expr ")"

arith ::=
  | MINUS INT
  | expr PLUS expr
  | expr MINUS expr
  | expr TIMES expr

boolean ::=
  | expr EQUAL expr
  | expr LESS expr

ty ::=
  | TBOOL
  | TINT
  | ty "->" ty
  | "(" ty ")"
-}
