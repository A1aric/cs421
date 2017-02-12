--- Getting Started
--- ===============

--- Relevant Files
--- --------------

module Main where

import System.IO (hFlush, stdout)

import Data.HashMap.Strict as H (HashMap, empty, fromList, insert, lookup, union)
import Data.Functor.Identity

import Text.ParserCombinators.Parsec hiding (Parser)
import Text.Parsec.Prim (ParsecT)

--- Given Code
--- ==========

--- Data Types
--- ----------

--- ### Environments and Results

type Env  = H.HashMap String Val
type PEnv = H.HashMap String Stmt

type Result = (String, PEnv, Env)

--- ### Values

data Val = IntVal Int
         | BoolVal Bool
         | CloVal [String] Exp Env
         | ExnVal String
    deriving (Eq)

instance Show Val where
    show (IntVal i) = show i
    show (BoolVal i) = show i
    show (CloVal xs body env) = "<" ++ show xs   ++ ", "
                                    ++ show body ++ ", "
                                    ++ show env  ++ ">"
    show (ExnVal s) = "exn: " ++ s

--- ### Expressions

data Exp = IntExp Int
         | BoolExp Bool
         | FunExp [String] Exp
         | LetExp [(String,Exp)] Exp
         | AppExp Exp [Exp]
         | IfExp Exp Exp Exp
         | IntOpExp String Exp Exp
         | BoolOpExp String Exp Exp
         | CompOpExp String Exp Exp
         | VarExp String
    deriving (Show, Eq)

--- ### Statements

data Stmt = SetStmt String Exp
          | PrintStmt Exp
          | QuitStmt
          | IfStmt Exp Stmt Stmt
          | ProcedureStmt String [String] Stmt
          | CallStmt String [Exp]
          | SeqStmt [Stmt]
    deriving (Show, Eq)

--- Primitive Functions
--- -------------------

intOps :: H.HashMap String (Int -> Int -> Int)
intOps = H.fromList [ ("+", (+))
                    , ("-", (-))
                    , ("*", (*))
                    , ("/", (div))
                    ]

boolOps :: H.HashMap String (Bool -> Bool -> Bool)
boolOps = H.fromList [ ("and", (&&))
                     , ("or", (||))
                     ]

compOps :: H.HashMap String (Int -> Int -> Bool)
compOps = H.fromList [ ("<", (<))
                     , (">", (>))
                     , ("<=", (<=))
                     , (">=", (>=))
                     , ("/=", (/=))
                     , ("==", (==))
                     ]

--- Parser
--- ------

-- Pretty name for Parser types
type Parser = ParsecT String () Identity

-- for testing a parser directly
run :: Parser a -> String -> a
run p s =
    case parse p "<stdin>" s of
        Right x -> x
        Left x  -> error $ show x

-- Lexicals

symbol :: String -> Parser String
symbol s = do string s
              spaces
              return s

int :: Parser Int
int = do digits <- many1 digit <?> "an integer"
         spaces
         return (read digits :: Int)

var :: Parser String
var = do v <- many1 letter <?> "an identifier"
         spaces
         return v

parens :: Parser a -> Parser a
parens p = do symbol "("
              pp <- p
              symbol ")"
              return pp

-- Expressions

intExp :: Parser Exp
intExp = do i <- int
            return $ IntExp i

boolExp :: Parser Exp
boolExp =    ( symbol "true"  >> return (BoolExp True)  )
         <|> ( symbol "false" >> return (BoolExp False) )

varExp :: Parser Exp
varExp = do v <- var
            return $ VarExp v

opExp :: (String -> Exp -> Exp -> Exp) -> String -> Parser (Exp -> Exp -> Exp)
opExp ctor str = symbol str >> return (ctor str)

mulOp :: Parser (Exp -> Exp -> Exp)
mulOp = let mulOpExp = opExp IntOpExp
        in  mulOpExp "*" <|> mulOpExp "/"

addOp :: Parser (Exp -> Exp -> Exp)
addOp = let addOpExp = opExp IntOpExp
        in  addOpExp "+" <|> addOpExp "-"

andOp :: Parser (Exp -> Exp -> Exp)
andOp = opExp BoolOpExp "and"

orOp :: Parser (Exp -> Exp -> Exp)
orOp = opExp BoolOpExp "or"

compOp :: Parser (Exp -> Exp -> Exp)
compOp = let compOpExp s = symbol s >> return (CompOpExp s)
         in     try (compOpExp "<=")
            <|> try (compOpExp ">=")
            <|> compOpExp "/="
            <|> compOpExp "=="
            <|> compOpExp "<"
            <|> compOpExp ">"

ifExp :: Parser Exp
ifExp = do try $ symbol "if"
           e1 <- expr
           symbol "then"
           e2 <- expr
           symbol "else"
           e3 <- expr
           symbol "fi"
           return $ IfExp e1 e2 e3

funExp :: Parser Exp
funExp = do try $ symbol "fn"
            symbol "["
            params <- var `sepBy` (symbol ",")
            symbol "]"
            body <- expr
            symbol "end"
            return $ FunExp params body

letExp :: Parser Exp
letExp = do try $ symbol "let"
            symbol "["
            params <- (do v <- var
                          symbol ":="
                          e <- expr
                          return (v,e)
                      )
                      `sepBy` (symbol ";")
            symbol "]"
            body <- expr
            symbol "end"
            return $ LetExp params body

appExp :: Parser Exp
appExp = do try $ symbol "apply"
            efn <- expr
            symbol "("
            exps <- expr `sepBy` (symbol ",")
            symbol ")"
            return $ AppExp efn exps

expr :: Parser Exp
expr = let disj = conj `chainl1` andOp
           conj = arith `chainl1` compOp
           arith = term `chainl1` addOp
           term = factor `chainl1` mulOp
           factor = atom
       in  disj `chainl1` orOp

atom :: Parser Exp
atom = intExp
   <|> funExp
   <|> ifExp
   <|> letExp
   <|> try boolExp
   <|> appExp
   <|> varExp
   <|> parens expr

-- Statements

quitStmt :: Parser Stmt
quitStmt = do try $ symbol "quit"
              symbol ";"
              return QuitStmt

printStmt :: Parser Stmt
printStmt = do try $ symbol "print"
               e <- expr
               symbol ";"
               return $ PrintStmt e

setStmt :: Parser Stmt
setStmt = do v <- var
             symbol ":="
             e <- expr
             symbol ";"
             return $ SetStmt v e

ifStmt :: Parser Stmt
ifStmt = do try $ symbol "if"
            e1 <- expr
            symbol "then"
            s2 <- stmt
            symbol "else"
            s3 <- stmt
            symbol "fi"
            return $ IfStmt e1 s2 s3

procStmt :: Parser Stmt
procStmt = do try $ symbol "procedure"
              name <- var
              symbol "("
              params <- var `sepBy` (symbol ",")
              symbol ")"
              body <- stmt
              symbol "endproc"
              return $ ProcedureStmt name params body

callStmt :: Parser Stmt
callStmt = do try $ symbol "call"
              name <- var
              symbol "("
              args <- expr `sepBy` (symbol ",")
              symbol ")"
              symbol ";"
              return $ CallStmt name args

seqStmt :: Parser Stmt
seqStmt = do try $ symbol "do"
             stmts <- many1 stmt
             symbol "od"
             symbol ";"
             return $ SeqStmt stmts

stmt :: Parser Stmt
stmt = quitStmt
   <|> printStmt
   <|> ifStmt
   <|> procStmt
   <|> callStmt
   <|> seqStmt
   <|> try setStmt

--- REPL
--- ----

repl :: PEnv -> Env -> [String] -> String -> IO Result
repl penv env [] _ =
  do putStr "> "
     hFlush stdout
     input <- getLine
     case parse stmt "stdin" input of
        Right QuitStmt -> do putStrLn "Bye!"
                             return ("",penv,env)
        Right x -> let (nuresult,nupenv,nuenv) = exec x penv env
                   in do {
                     putStrLn nuresult;
                     repl nupenv nuenv [] "stdin"
                   }
        Left x -> do putStrLn $ show x
                     repl penv env [] "stdin"

main :: IO Result
main = do
  putStrLn "Welcome to your interpreter!"
  repl H.empty H.empty [] "stdin"


--- Problems
--- ========

--- Lifting Functions
--- -----------------

liftIntOp :: (Int -> Int -> Int) -> Val -> Val -> Val
-- case of division by 0 must be checked first for pattern matching
liftIntOp div   _          (IntVal 0) = ExnVal "Division by 0"
liftIntOp op    (IntVal x) (IntVal y) = IntVal $ op x y
liftIntOp _     _           _         = ExnVal "Cannot lift"

liftBoolOp :: (Bool -> Bool -> Bool) -> Val -> Val -> Val
liftBoolOp op (BoolVal x) (BoolVal y) = BoolVal $ op x y
liftBoolOp _  _           _           = ExnVal "Cannot lift"

liftCompOp :: (Int -> Int -> Bool) -> Val -> Val -> Val
liftCompOp op (IntVal x) (IntVal y) = BoolVal $ op x y
liftCompOp _  _          _          = ExnVal "Cannot lift"
--- Eval
--- ----

eval :: Exp -> Env -> Val

--- ### Constants
eval (IntExp x)  env = IntVal x
eval (BoolExp x) env = BoolVal x

--- ### Variables
eval (VarExp x) env = case H.lookup x env of
                      Nothing -> ExnVal "No match in env"
                      Just v -> v

--- ### Arithmetic
eval (IntOpExp op x y) env =
    let Just v = H.lookup op intOps
    in liftIntOp v (eval x env) (eval y env)

--- ### Boolean and Comparison Operators
eval (BoolOpExp op x y) env =
    let Just v = H.lookup op boolOps
    in liftBoolOp v (eval x env) (eval y env)

eval (CompOpExp op x y) env =
    let Just v = H.lookup op compOps
    in liftCompOp v (eval x env) (eval y env)

--- ### If Expressions
eval (IfExp op x y) env = case eval op env of
                          BoolVal True -> eval x env
                          BoolVal False -> eval y env
                          _     -> ExnVal "Condition is not a Bool"

--- ### Functions and Function Application
eval (FunExp xx y) env = CloVal xx y env

eval (AppExp func xs) env = case eval func env of
                            (CloVal ys newfunc newenv) -> eval (newfunc) (H.union (H.fromList (zip ys (map (\zs -> eval zs env) xs))) newenv)
                            _                        -> ExnVal "Apply to non-closure"

--- ### Let Expressions
eval (LetExp xs e) env = eval e (H.union (H.fromList (zip (map (\(a, b) -> a) xs) (map (\(a, b) -> eval b env) xs))) env)

--- Statements
--- ----------

exec :: Stmt -> PEnv -> Env -> Result
exec (PrintStmt e) penv env = (val, penv, env)
    where val = show $ eval e env

--- ### Set Statements
exec (SetStmt x e) penv env = ("", penv, H.insert x (eval e env) env)

--- ### Sequencing
exec (SeqStmt [])     penv env = ("", penv, env)
exec (SeqStmt (x:xs)) penv env = (y1 ++ y2, newnewpenv, newnewenv)
            where
                (y1, newpenv,    newenv)    = exec x            penv    env
                (y2, newnewpenv, newnewenv) = exec (SeqStmt xs) newpenv newenv

--- ### If Statements
exec (IfStmt e s1 s2) penv env = case eval e env of
                                 BoolVal True  -> exec s1 penv env
                                 BoolVal False -> exec s2 penv env
                                 _             -> ("exn: Condition is not a Bool", penv, env)

--- ### Procedure and Call Statements
exec (ProcedureStmt a xs b) penv env = ("", H.insert (a) (ProcedureStmt a xs b) penv, env)


exec (CallStmt a (xx))    penv env = case (H.lookup a penv) of
                                       Nothing                        -> ("Procedure " ++ a ++ " undefined", penv, env)
                                       Just (ProcedureStmt name ys c) -> exec (c) penv (H.union (H.fromList (zip ys (map (\z -> eval z env) xx))) (env))
