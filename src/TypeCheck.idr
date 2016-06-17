module TypeCheck

import Syntax

public export
TypeError : Type
TypeError = String
 
typeError : TypeError -> Either TypeError Ty
typeError msg = Left msg

||| Type context.
public export
Ctx : Type
Ctx = List (Name, Ty)

mutual
  check : Ctx -> Ty -> Expr -> Either TypeError Ty
  check ctx ty e = do
    ty' <- typeOf ctx e
    if ty' /= ty 
    then typeError ((show e) ++ " has type " ++ (show ty') ++ " but is used as if it has type " ++ (show ty))
    else pure ty

  ||| `typeOf ctx e` computes the type of expression `e` in 
  ||| type context `ctx`. If `e` does not have a type it 
  ||| returns a `TypeError`.
  export
  typeOf : Ctx -> Expr -> Either TypeError Ty
  typeOf ctx e = case e of
    Var name => case lookup name ctx of
                  Nothing => typeError ("unknown variable " ++ name)
                  Just ty => Right ty
    EInt _  => Right TInt
    EBool _ => Right TBool
    Times e1 e2 => do check ctx TInt e1
                      check ctx TInt e2
                      pure TInt
    Plus e1 e2  => do check ctx TInt e1
                      check ctx TInt e2
                      pure TInt
    Minus e1 e2 => do check ctx TInt e1
                      check ctx TInt e2
                      pure TInt
    Equal e1 e2 => do check ctx TInt e1
                      check ctx TInt e2
                      pure TBool
    Less e1 e2  => do check ctx TInt e1
                      check ctx TInt e2
                      pure TBool

    If e1 e2 e3 => do check ctx TBool e1
                      ty <- typeOf ctx e2
                      check ctx ty e3
                      pure ty

    Fun f x pty rty e => let ectx = (f, TArrow pty rty) :: (x, pty) :: ctx in
                         do check ectx rty e
                            pure (TArrow pty rty)

    Apply e1 e2 =>  do ty1 <- typeOf ctx e1
                       case ty1 of
                         TArrow pty rty => do check ctx pty e2
                                              pure rty
                         ty => typeError $ (show e1) ++ " is used as a function but its type is " ++ (show ty)
