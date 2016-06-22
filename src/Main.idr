module Main

import TypeCheck
import Machine
import Syntax
import Compile
import Lexer
import Parser

import Effects
import Effect.StdIO

import Control.Monad.Identity
import Lightyear as Ly
import Lightyear.Core as LyC
import Lightyear.Strings as LyS

quit : String
quit = ":q"
  
prompt : String
prompt = "MiniML> "

welcome : String
welcome = "MiniML. Use " ++ quit ++ " or ^D to quit."

farewell : String
farewell = "Goodbye."

implementation Layout (List Lexeme) where
  lineLengths xs = map (toIntNat . Prelude.Strings.length) . lines $ show xs

lex : String -> Either String (List Lexeme)
lex i = let Id r = LyC.execParserT lexer i in
  case r of
    Success "" lexemes => Right lexemes
    Success left _     => Left $ "Unexpected EOS while lexing: " ++ (show left)
    Failure es         => Left $ formatError i es

parse : List Lexeme -> Either String (List TopLevelCmd)
parse i = let Id r = LyC.execParserT toplevel i in
  case r of
    Success [] cmds    => Right cmds
    Success lexemes _  => Left ("Unexpected EOS while parsing: " ++ (show lexemes))
    Failure es         => Left $ formatError i es

runEither : Frame -> Env -> Either MachineError (MValue, Env)
runEither frm env = the (Either MachineError (MValue, Env)) (mrun frm env)

ErrorMsg : Type
ErrorMsg = String

execCmd : (Ctx, Env) -> TopLevelCmd -> Either ErrorMsg ((Ctx, Env), String)
execCmd (ctx, env) cmd = case cmd of
  TopExpr e => do
    ty <- typeOf ctx e
    let frm = compile e
    (v, env') <- runEither frm env
    pure $ case e of
      Fun name _ pTy rTy body => (((name, ty) :: ctx, (name, v) :: env'), name ++ " : " ++ show ty ++ " = " ++ show v)
      _                       => ((ctx, env'), "- : " ++ show ty ++ " = " ++ show v)
  TopDef name e => do
    ty <- typeOf ctx e
    let frm = compile e
    (v, env') <- runEither frm env
    pure $ (((name, ty) :: ctx, (name, v) :: env'),
            name ++ " : " ++ show ty ++ " = " ++ show v)

execCmds : (Ctx, Env) -> List TopLevelCmd -> IO (Ctx, Env)
execCmds ctxEnv Nil = pure ctxEnv
execCmds ctxEnv (cmd :: cmds) =
  case execCmd ctxEnv cmd of
    Right (ctxEnv', result) => do putStrLn result
                                  ctxEnv'' <- execCmds ctxEnv' cmds
                                  pure ctxEnv''
    Left errMsg             => do putStrLn ("Error: " ++ errMsg)
                                  pure ctxEnv

printTyPair : (Name, Ty) -> IO ()
printTyPair (name, ty) = putStrLn ("  " ++ name ++ " : " ++ show ty)

printTypeEnv : Ctx -> IO ()
printTypeEnv ctx = do
  putStrLn "typing environment {"
  for_ ctx printTyPair
  putStrLn "}"

printValPair : (MName, Lazy MValue) -> IO ()
printValPair (name, v) = putStrLn $ "  " ++ name ++ " : " ++ show (Force v)

printValueEnv : Env -> IO ()
printValueEnv env = do
  putStrLn "value environment {"
  for_ env printValPair
  putStrLn "}"
  
debugEnvirons : Bool
debugEnvirons = False

mutual
  myRepl : (Ctx, Env) -> IO ()
  myRepl ctxEnv = do
    when (debugEnvirons) (case ctxEnv of
      (ctx, env) => do printTypeEnv ctx
                       printValueEnv env
    )
    putStr prompt
    line <- getLine
    isEof <- fEOF stdin
    if isEof then putStrLn ""
    else if line == quit then pure ()
    else case line of
           ""   => myRepl ctxEnv
           line => case lex line >>= parse of
                     Right cmds => do ctxEnv' <- execCmds ctxEnv cmds
                                      myRepl ctxEnv'
                     Left err   => do putStrLn $ "Error: " ++ err
                                      myRepl ctxEnv

interactive : IO ()
interactive = do
  putStrLn welcome
  myRepl ([], [])
  putStrLn farewell

execFile : (Ctx, Env) -> String -> IO ()
execFile ctxEnv contents =
  case lex contents >>= parse of
    Right cmds => do _ <- execCmds ctxEnv cmds
                     pure ()
    Left err   => putStrLn $ "Error: " ++ err

main : IO ()
main = do
  args <- getArgs
--  putStrLn $ show args
  case args of
    [prog, filename] => do r <- readFile filename
                           case r of
                             Right contents => execFile ([], []) contents
                             Left _         => putStrLn $ "Cannot read " ++ filename
    [prog]           => interactive
    _                => putStrLn "Usage: miniml [filename]"
