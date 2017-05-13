{-# LANGUAGE FlexibleContexts #-}

module Scheme.Runtime where

import Scheme.Core
import Scheme.Parse
import Scheme.Eval

import qualified Data.HashMap.Strict as H
import Text.ParserCombinators.Parsec hiding (Parser, State)
import Control.Monad
import Control.Monad.State
import Control.Monad.Except
import Data.Foldable

--- ### Helper functions for lifting and lowering

lowerBool :: Val -> Bool
lowerBool (Boolean False) = False
lowerBool _ = True

lowerInt :: Val -> EvalState Int
lowerInt (Number i) = return i
lowerInt v = throwError $ TypeError v

lowerList :: Val -> EvalState [Val]
lowerList (List xx) = return xx
lowerList v = throwError $ TypeError v

liftIntVargOp :: (Int -> Int -> Int) -> Int -> Val
liftIntVargOp f c = PrimFunc p where
    p [] = return $ Number c
    p [x] = Number . f c <$> lowerInt x
    p xx = Number . foldl1 f <$> mapM lowerInt xx

liftBoolVargOp :: ([Bool] -> Bool) -> Val
liftBoolVargOp f = PrimFunc $ return . Boolean . f . map lowerBool

-- TODO
liftIntBinOp :: (Int -> Int -> Int) -> Val
liftIntBinOp f = PrimFunc p where
    p [(Number x),(Number y)] = return $ Number (f x y)
    p xx    = throwError $ UnexpectedArgs xx
    -- You should replace the following line with your own implementation
    -- PrimFunc . const $ unimplemented "Lifting binary integer operator (`liftIntBinOp`)"


liftIntUnaryOp :: (Int -> Int) -> Val
liftIntUnaryOp f = PrimFunc p where
    p [Number x] = return $ Number $ f x
    p v = throwError $ UnexpectedArgs v
  -- You should replace the following line with your own implementation
  -- PrimFunc . const $ unimplemented "Lifting unary integer operator (`liftIntUnaryOp`)"

liftBoolUnaryOp :: (Bool -> Bool) -> Val
liftBoolUnaryOp f = PrimFunc p where
    p [Boolean False] = return $ Boolean $ f False
    p [_] = return $ Boolean $ f True
    p v = throwError $ UnexpectedArgs v


liftCompOp :: (Int -> Int -> Bool) -> Val
liftCompOp f = PrimFunc p where
    p [] = return $ Boolean True
    p [x] = return $ Boolean True
    p xx = Boolean . myFold f True <$> mapM lowerInt xx
  -- You should replace the following line with your own implementation
  -- PrimFunc . const $ unimplemented "Lifting comparison operator (`liftCompOp`)"


myFold :: (a -> a -> Bool) -> Bool -> [a] -> Bool
myFold f z []       = z
myFold f z [_]      = z
myFold f z (x:y:xs) = (f x y) && (myFold f z xs)

--- ### Primtive operations

--worked with ymostofi

-- Primitive function `car`
car :: [Val] -> EvalState Val
car xx = if length xx == 1
    then case (flattenList (head xx)) of
        List x -> return $ head x
        DottedList (a:as) sa -> return a
        otherwise -> throwError $ UnexpectedArgs xx
    else throwError $ UnexpectedArgs xx
-- Primitive function `cdr`
cdr :: [Val] -> EvalState Val
-- cdr = const $ unimplemented "Primitive function `cdr`"
cdr xx = if length xx == 1
    then case (flattenList (List (tail xx))) of
        List x -> return $ List (tail x)
        DottedList (a:as) sa -> return a
        otherwise -> throwError $ UnexpectedArgs xx
    else throwError $ UnexpectedArgs xx

-- Primitive function `cons`
cons :: [Val] -> EvalState Val
cons xx = if length xx == 2
  then return $ (DottedList [(head xx)] (head $ tail xx))
  else throwError $ UnexpectedArgs xx

list :: [Val] -> EvalState Val
list xx = return $ List xx

-- Primitive function `append`
append :: [Val] -> EvalState Val
append [] = return $ List []
append [x] = return x
append vv = foldlM append' (List []) (map flattenList vv) where
    append' (List []) x = return x
    append' (List xs) (List ys) = return $ List (xs ++ ys)
    append' (List xs) (DottedList ys y) = return $ DottedList (xs ++ ys) y
    append' _ acc = throwError $ TypeError acc

-- Primitive function `apply`
-- It applies a function to a list of parameters
-- Examples:
--   (apply + '(1 2 3))  => 6
--   (apply car '((1 2 3)))  => 1
applyPrim :: [Val] -> EvalState Val
applyPrim [f, List args] = apply f args
applyPrim v              = throwError $ UnexpectedArgs v

-- Primitive function `eval`
-- It evaluates the single argument as an expression
-- All you have to do is to check the number of arguments and
-- feed the single argument to the evaluator!
-- TODO
-- Examples:
--   (eval '(+ 1 2 3))  => 6
evalPrim :: [Val] -> EvalState Val
evalPrim xx | length xx == 1 = eval $ head xx
            | otherwise = throwError $ UnexpectedArgs xx

-- Primitive function `=`, throwing type error for mismatch
-- `=` is a comparison operator for numbers and booleans
-- TODO
-- Examples:
--   (= 1 1) => #t
--   (= #f #t) => #f
--   (= #f #f) => #t
--   (= 'a 10) => Type error
--   (= 'a 'b) => Type error
equalSign :: [Val] -> EvalState Val
equalSign [] = return $ Boolean True
equalSign [x] = return $ Boolean True
equalSign (x:xs) = aux_s xs x True

aux_s []              (Number i) b = return $ Boolean b
aux_s ((Number x):xs) (Number i) b = aux_s xs (Number i) (b && (x == i))
aux_s []              (Boolean i) b = return $ Boolean b
aux_s ((Boolean x):xs) (Boolean i) b = aux_s xs (Boolean i) (b && (x == i))
aux_s (x:xs) _ _ = throwError $ TypeError x

-- Primitive function `eq?`, not throwing any error
-- `eq?` is a comparison operator for atom values (numbers, booleans, and symbols)
-- Returns `#f` on type mismatch or unsupported types (functions etc)
-- TODO
-- Examples:
--   (eq? 1 1) => #t
--   (eq? #f #t) => #f
--   (eq? #f #f) => #t
--   (eq? 'a 10) => #f
--   (eq? 'a 'a) => #t
eq :: [Val] -> EvalState Val
eq [] = return $ Boolean True
eq [x] = return $ Boolean True
eq (x:xs) = aux xs x True

aux []              (Number i) b = return $ Boolean b
aux ((Number x):xs) (Number i) b = aux xs (Number i) (b && (x == i))
aux []              (Boolean i) b = return $ Boolean b
aux ((Boolean x):xs) (Boolean i) b = aux xs (Boolean i) (b && (x == i))
aux []              (Symbol i) b = return $ Boolean b
aux ((Symbol x):xs) (Symbol i) b = aux xs (Symbol i) (b && (x == i))
aux (x:xs) _ _ = return $ Boolean False



-- Primitive function `list?` predicate
-- `(list? arg)` determines whether `arg` is a non-dotted list
-- or an empty list (null)
isList :: [Val] -> EvalState Val
isList [v] = return . Boolean $ case flattenList v of
                    List _ -> True
                    _ -> False
isList vv = throwError $ UnexpectedArgs vv

-- Primitive function `symbol?` predicate
isSymbol :: [Val] -> EvalState Val
isSymbol [Symbol _] = return $ Boolean True
isSymbol [_] = return $ Boolean False
isSymbol vv = throwError $ UnexpectedArgs vv

-- Primitive function `pair?` predicate
-- Any `List` or `DottedList` is a pair
isPair :: [Val] -> EvalState Val
isPair [x] = return . Boolean $ case flattenList x of
  List x -> not (length x == 0)
  DottedList ys y -> True
  _ -> False
isPair xx = throwError $ UnexpectedArgs xx

-- Primitive function `number?` predicate
-- TODO
isNumber :: [Val] -> EvalState Val
isNumber [Number _] = return $ Boolean True
isNumber [_] = return $ Boolean False
isNumber vv = throwError $ UnexpectedArgs vv

-- Primitive function `boolean?` predicate
isBoolean :: [Val] -> EvalState Val
isBoolean [Boolean _] = return $ Boolean True
isBoolean [_] = return $ Boolean False
isBoolean vv = throwError $ UnexpectedArgs vv
    --const $ unimplemented "Primitive function `boolean?`"

-- Primitive function `null?` predicate
-- An empty list or its *equivalent* value is null
-- Note: Think about what's equivalent
-- TODO
isNull :: [Val] -> EvalState Val
isNull [x] =  return . Boolean $ case flattenList x of
                            List x -> (length x == 0)
                            _      -> False
isNull vv = throwError $ UnexpectedArgs vv
--- ### Runtime

runtime :: Env
runtime = H.fromList [ ("+", liftIntVargOp (+) 0)
                     , ("-", liftIntVargOp (-) 0)
                     , ("*", liftIntVargOp (*) 1)
                     , ("/", liftIntVargOp (div) 1)
                     , ("and", liftBoolVargOp and)
                     , ("or", liftBoolVargOp or)
                     , ("not", liftBoolUnaryOp not)
                     , ("abs", liftIntUnaryOp abs)
                     , ("modulo", liftIntBinOp mod)
                     , (">", liftCompOp (>))
                     , ("<", liftCompOp (<))
                     , (">=", liftCompOp (>=))
                     , ("<=", liftCompOp (<=))
                     , ("!=", liftCompOp (/=))
                     , ("cons", PrimFunc cons)
                     , ("append", PrimFunc append)
                     , ("car", PrimFunc car)
                     , ("list", PrimFunc list)
                     , ("cdr", PrimFunc cdr)
                     , ("eval", PrimFunc evalPrim)
                     , ("apply", PrimFunc applyPrim)
                     , ("=", PrimFunc equalSign)
                     , ("eq?", PrimFunc eq)
                     , ("list?", PrimFunc isList)
                     , ("symbol?", PrimFunc isSymbol)
                     , ("pair?", PrimFunc isPair)
                     , ("number?", PrimFunc isNumber)
                     , ("boolean?", PrimFunc isBoolean)
                     , ("null?", PrimFunc isNull)
                     ]
