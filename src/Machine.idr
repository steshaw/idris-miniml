module Machine

import Syntax as S
import Effects
import Effect.Exception
import Data.List

public export
MName : Type
MName = S.Name

mutual
  export
  data MValue
    = MInt Integer
    | MBool Bool
    | MClosure MName Frame Env

  export
  implementation Show MValue where
    show (MInt n)         = show n
    show (MBool b)        = show b
    show (MClosure _ _ _) = "<fun>"

  public export
  data Instr
    = IMult
    | IAdd
    | ISub
    | IEqual
    | ILess
    | IVar MName
    | IInt Integer
    | IBool Bool
    | IClosure MName MName Frame
    | IBranch Frame Frame
    | ICall
    | IPopEnv

  ||| A stack of instructions.
  public export
  Frame : Type 
  Frame = List Instr

  public export
  Env : Type
  Env = List (MName, Lazy MValue)

  Stack : Type
  Stack = List MValue

public export
MachineError : Type
MachineError = String

public export
MEff : Type -> Type
--MEff a = Eff a [EXCEPTION MachineError]
MEff a = Either MachineError a

error : String -> MEff b
--error msg = raise (MkMachineError msg)
error msg = Left msg

Environs : Type
Environs = List Env

lookup : MName -> Environs -> MEff MValue
lookup x envs = case head' envs of
  Just env => case lookup x env of
                Just v  => pure v
                Nothing => error $ "Unknown " ++ x
  Nothing  => error $ "No environment in which to lookup " ++ x

pop : Stack -> MEff (MValue, Stack)
pop [] = error "Empty stack"
pop (v :: vs) = pure $ (v, vs)

popBool : Stack -> MEff (Bool, Stack)
popBool (MBool b :: vs) = pure (b, vs)
popBool _               = error "Bool expected"

||| Pop a value and a closure from a stack
popApp : Stack -> MEff (MName, Frame, Env, MValue, Stack)
popApp (v :: MClosure n fr env :: vs) = pure (n, fr, env, v, vs)
popApp _                           = error "Value and closure expected"

mult : Stack -> MEff Stack
mult (MInt i2 :: MInt i1 :: vs) = pure $ MInt (i1 * i2) :: vs
mult _                          = error "int and int expected in mult"

add : Stack -> MEff Stack
add (MInt i2 :: MInt i1 :: vs) = pure $ MInt (i1 + i2) :: vs
add _                          = error "int and int expected in add"

sub : Stack -> MEff Stack
sub (MInt i2 :: MInt i1 :: vs) = pure $ MInt (i1 - i2) :: vs
sub _                          = error "int and int expected in sub"

equal : Stack -> MEff Stack
equal (MInt i2 :: MInt i1 :: vs) = pure $ MBool (i1 == i2) :: vs
equal _                          = error "int and int expected in equal"

less : Stack -> MEff Stack
less (MInt i2 :: MInt i1 :: vs) = pure $ MBool (i1 < i2) :: vs
less _                          = error "int and int expected in less"
 
Frames : Type
Frames = List Frame

buildClosure : Frames -> Stack -> Environs -> Env -> MName -> MName -> Frame -> (Frames, Stack, Environs)
buildClosure frames stack envs env fnm nm frame =
  (frames, c :: stack, envs)
    where c = MClosure nm frame ((fnm, c) :: env)

exec : Instr -> Frames -> Stack -> Environs -> MEff (Frames, Stack, Environs)
exec instr frames stack envs = case instr of
  -- Arithmetic operations.
  IMult => do stack' <- mult stack
              pure (frames, stack', envs)
  IAdd  => do stack' <- add stack
              pure (frames, stack', envs)
  ISub  => do stack' <- sub stack
              pure (frames, stack', envs)
  IEqual=> do stack' <- equal stack
              pure (frames, stack', envs)
  ILess => do stack' <- less stack
              pure (frames, stack', envs)
  -- Pushing values onto the stack.
  IVar nm  => do v <- lookup nm envs
                 pure $ (frames, v :: stack, envs)
  IInt n   => pure $ (frames, MInt n :: stack, envs)
  IBool b  => pure $ (frames, MBool b :: stack, envs)

  IClosure f nm frame =>
    case head' envs of
      Just env => pure $ buildClosure frames stack envs env f nm frame
      Nothing  => error "No environment for a closure"

  -- Control instructions.
  IBranch f1 f2 => do (b, stack') <- popBool stack
                      pure $ ((if b then f1 else f2) :: frames, stack', envs)

  ICall => do (nm, frame, env, v, stack') <- popApp stack
              pure $ (frame :: frames, stack', ((nm, v) :: env) :: envs)

  IPopEnv => case envs of
    _ :: envs' => pure (frames, stack, envs')
    _          => error "No environment to pop"

export
mrun : Frame -> Env -> MEff (MValue, Env)
mrun frame env = loop ([frame], [], [env])
  where 
    loop : (Frames, Stack, Environs) -> MEff (MValue, Env)
    loop ([], [v], [env]) = pure (v, env)
    loop ((i::is) :: frames, stack, envs) = do r <- exec i (is::frames) stack envs
                                               loop r
    loop ([] :: frames, stack, envs) = loop (frames, stack, envs)
