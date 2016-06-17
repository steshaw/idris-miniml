module Compile

import Syntax
import Machine

export
compile : Expr -> List Instr
compile (Var nm) = [IVar nm]
compile (EInt i) = [IInt i]
compile (EBool b) = [IBool b]
compile (Times e1 e2) = compile e1 ++ compile e2 ++ [IMult]
compile (Plus e1 e2) = compile e1 ++ compile e2 ++ [IAdd]
compile (Minus e1 e2) = compile e1 ++ compile e2 ++ [ISub]
compile (Equal e1 e2) = compile e1 ++ compile e2 ++ [IEqual]
compile (Less e1 e2) = compile e1 ++ compile e2 ++ [ILess]
compile (If e1 e2 e3) = compile e1 ++ [IBranch (compile e2) (compile e3)]
compile (Fun fnm nm _ _ body) = [IClosure fnm nm (compile body ++ [IPopEnv])]
compile (Apply e1 e2) = (compile e1) ++ (compile e2) ++ [ICall]
