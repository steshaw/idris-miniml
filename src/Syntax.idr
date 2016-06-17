module Syntax

||| Types
public export
data Ty : Type where
  ||| Integers
  TInt : Ty
  ||| Booleans
  TBool : Ty
  ||| Functions
  TArrow : Ty -> Ty -> Ty

export
implementation Eq Ty where
  TInt == TInt               = True
  TBool == TBool             = True
  (TArrow p1 r1) == (TArrow p2 r2) = p1 == p2 && r1 == r2
  _ == _                       = False

export
implementation Show Ty where
  show TInt  = "int"
  show TBool = "bool"
  show (TArrow pty rty) = "(" ++ (show pty) ++ " → " ++ (show rty) ++ ")"

public export
Name : Type
Name = String

public export
data Expr : Type where
  ||| Variables
  Var : Name -> Expr
  ||| Non-negative "integer" constant
  EInt : Integer -> Expr
  ||| Boolean constant
  EBool : Bool -> Expr
  ||| Product `e1 * e2`
  Times : Expr -> Expr -> Expr
  ||| Sum `e1 + e2`
  Plus : Expr -> Expr -> Expr
  ||| Difference `e1 - e2`
  Minus : Expr -> Expr -> Expr
  ||| Int comparison `e1 = e2`
  Equal : Expr -> Expr -> Expr
  ||| Int comparison `e1 < e2`
  Less : Expr -> Expr -> Expr
  ||| Conditional `if e1 then e2 else e3`
  If : Expr -> Expr -> Expr -> Expr
  ||| Function `fun f(x:s):t is e`
  Fun : Name -> Name -> Ty -> Ty -> Expr -> Expr
  ||| Application `e1 e2`
  Apply : Expr -> Expr -> Expr

export
implementation Show Expr where
  show (Var name) = "(Var " ++ name ++ ")"
  show (EInt n)   = show n
  show (EBool b)  = show b
  show (Times e1 e2) = "(" ++ show e1 ++ " * " ++ show e2 ++ ")"
  show (Plus e1 e2) = "(" ++ show e1 ++ " + " ++ show e2 ++ ")"
  show (Minus e1 e2) = "(" ++ show e1 ++ " - " ++ show e2 ++ ")"
  show (Equal e1 e2) = "(" ++ show e1 ++ " = " ++ show e2 ++ ")"
  show (Less e1 e2) = "(" ++ show e1 ++ " < " ++ show e2 ++ ")"
  show (If c te fe) = "if" ++ show c ++ " then " ++ show te ++ " else " ++ show fe
  show (Fun fnm pnm pty rty body) = "fun " ++ show fnm ++ " (" ++ pnm ++ " : " ++ show pty ++ ") : " ++ show (rty) ++ " is " ++ (show body)
  show (Apply e1 e2) = "(" ++ show e1 ++ " " ++ show e2 ++ ")"

public export
data TopLevelCmd : Type where
  ||| Expression
  TopExpr : Expr -> TopLevelCmd
  ||| Value definition `let x = e`
  TopDef : Name -> Expr -> TopLevelCmd
