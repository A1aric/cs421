{-# LANGUAGE FlexibleContexts, LambdaCase #-}

module Scheme.Eval where

import Scheme.Core

import Prelude hiding (lookup)
import qualified Data.HashMap.Strict as H (HashMap, insert, lookup, empty, fromList, union)
import Control.Monad.State
import Control.Monad.Except

-- ### Evaluation helpers

-- Evaluates a symbol to string
-- Throws an error if value is not a symbol
-- Examples:
--   getSym (Symbol "x")  ==> "x"
--   getSym (Number 1)    ==> Not a symbol: x
getSym :: Val -> EvalState String
getSym (Symbol x) = return x
getSym         v  = throwError $ NotASymbol v

-- `let` and `let*`
getBinding :: Val -> EvalState (String, Val)
getBinding (List [c, e]) = liftM2 (,) (getSym c) (eval e)
getBinding v = throwError $ NotAListOfTwo v

-- Evaluates a list of two to a tuple
-- Throws an error if value is not a list of two
-- This is useful in special form `cond`, since each clause
-- is expected to be exactly a two-element list
-- Examples:
--   getListOf2 (List (Number 1) (Symbol "x"))
--     ==> ((Number 1), (Symbol "x"))
--   getListOf2 (List (Number 1))
--     ==> Not a list of two elements: (1)
getListOf2 :: Val -> EvalState (Val, Val)
getListOf2 (List [c, e]) = return (c, e)
getListOf2 v = throwError $ NotAListOfTwo v

--- ### Keywords

-- When evaluating special forms, a list form starting with a keyword
-- is expected to match the special form syntax.
keywords :: [String]
keywords = [ "define"
           , "lambda"
           , "cond"
           , "let"
           , "let*"
           , "define-macro"
           , "quasiquote"
           , "unquote"
           ]

-- ### The monadic evaluator
-- Unlike evaluators in previous MPs, `eval` does not take any environment!
-- This is because the environment is encapsulated in the `EvalState` monad.
-- To access the environment, all you have to do is `get`, `modify` or `put`!
eval :: Val -> EvalState Val

-- Self-evaluating expressions
-- TODO: What's self-evaluating?
eval v@(Number _) = return v
eval v@(Boolean _) = return v

-- Symbol evaluates to the value bound to it
-- TODO
eval (Symbol sym) =
    do  t_env   <-  get
        let a   =   H.lookup sym t_env
        case a of
            Just b  -> return b
            Nothing -> throwError $ UndefSymbolError sym

-- Dotted list may just be an equivalent representation of List.
-- We simply try to flatten the list. If it's still dotted after
-- flattening, it's an invalid expression.
eval expr@(DottedList _ _) = case flattenList expr of
    DottedList _ _ -> throwError $ InvalidExpression expr
    v -> eval v

-- List evaluates as a form of the following
-- 1. Special form (`define`, `let`, `let*`, `cond`, `quote`, `quasiquote`,
--    `unquote`, `define-macro`, ...)
-- 2. Macro expansion (Macro)
-- 3. Function application (Func)
-- 4. Primitive function application (PrimFunc)
eval expr@(List lst) = evalList $ map flattenList lst where
    --- Evaluator for forms
    invalidSpecialForm :: String -> EvalState e
    invalidSpecialForm frm = throwError $ InvalidSpecialForm frm expr

    evalList :: [Val] -> EvalState Val

    evalList [] = throwError $ InvalidExpression expr

    -- quote
    -- TODO
    evalList [Symbol "quote", e] = --unimplemented "Special form `quote`"
        return e
    -- unquote (illegal at surface evaluation)
    -- TODO: since surface-level `unquote` is illegal, all you need to do is
    -- to throw a diagnostic
    evalList [Symbol "unquote", e] = --unimplemented "Special form `unquote`"
        throwError $ UnquoteNotInQuasiquote e

    -- quasiquote
    evalList [Symbol "quasiquote", e] = evalQuasi 1 e where
        evalQuasi :: Int -> Val -> EvalState Val
        evalQuasi 0 (List [Symbol "unquote", v]) = throwError $ UnquoteNotInQuasiquote v
        evalQuasi 1 (List [Symbol "unquote", v]) = eval v
        evalQuasi n (List ee@[Symbol "quasiquote", _]) = List <$> evalQuasi (n+1) `mapM` ee
        evalQuasi n (List ee@[Symbol "unquote", _]) = List <$> evalQuasi (n-1) `mapM` ee
        evalQuasi n (List xx) = List <$> mapM (evalQuasi n) xx
        evalQuasi n (DottedList xx y) = DottedList <$> mapM (evalQuasi n) xx <*> evalQuasi n y
        evalQuasi _ v = return v

    -- Why comment these out? Because `if` can be defined as a macro!
    -- -- if-then
    -- evalList [Symbol "if", condE, thenE] =
    --   eval condE >>= \c -> if lowerBool c then eval thenE else return Void
    -- -- if-then-else
    -- evalList [Symbol "if", condE, thenE, elseE] =
    --   eval condE >>= \c -> eval $ if lowerBool c then thenE else elseE

    -- cond
    -- TODO: Handle `cond` here. Use pattern matching to match the syntax
    evalList [Symbol "cond", List (exps)] | length exps >= 1 = unimplemented "cond"


    -- let
    -- TODO: Handle `let` here. Use pattern matching to match the syntax
    evalList [Symbol "let", List (exps), body] = --unimplemented "let"
        do  let temp = H.empty
            val     <- (\[(k, v)] -> H.insert k v temp) <$> mapM getBinding exps
            modify $ H.union temp
            res     <- eval body
            return res

    -- let*
    -- TODO: Handle `let*` here. Use pattern matching to match the syntax
    evalList [Symbol "let*", List (x:xs), List (e:es), body] = unimplemented "let*"

    -- lambda
    -- TODO: Handle `lambda` here. Use pattern matching to match the syntax
    evalList [Symbol "lambda", List (params), body] = --unimplemented "lambda"
        do  env <- get
            val <- (\argVal -> Func argVal body env) <$> mapM getSym params
            return $ (Func (map show params) body env)
            -- return val

    -- define function
    evalList [Symbol "define", List (Symbol fname : args), body] =
        do  env <- get
            val <- (\argVal -> Func argVal body env) <$> mapM getSym args
            modify $ H.insert fname val
            return Void

    -- define variable
    -- TODO: Handle `define` for variables here. Use pattern matching
    -- to match the syntax
    evalList [Symbol "define", Symbol var, body] =
        do  env <- get
            val <- eval body
            modify $ H.insert var val
            return Void


    -- define-macro
    -- TODO: Handle `define-macro` here. Use pattern matching to match
    -- the syntax
    evalList [Symbol "define-macro", List (Symbol fname : args), body] = -- unimplemented "define-macro"
        do  -- env <- get
            val <- eval body
            modify $ H.insert fname (Macro (map show args) body)
            return Void

    -- invalid use of keyword, throw a diagnostic
    evalList (Symbol sym : _) | elem sym keywords = invalidSpecialForm sym

    -- application
    evalList (fexpr:args) = eval fexpr >>= aux where
        -- Macro expansion
        -- TODO: implement macro evaluation
        -- Use do-notation!
        aux (Macro fmls body) | length fmls == length args = unimplemented "Macro expansion"
            -- do  old_env <- get
            --     fmap (\(k,v) -> modify $ H.insert k v) (zip args fmls)
            --     res <- eval fmls
            --     put old_env
            --     return $ eval res

        -- Function application
        -- TODO: evaluate arguments, and feed `f` along with the evaluated
        -- arguments to `apply`
        aux f =
            do  env <- get
                eval_args <- mapM eval args
                apply f (eval_args)

eval val = throwError $ InvalidExpression val

-- Function application
apply :: Val -> [Val] -> EvalState Val
-- Function
-- TODO: implement function application
-- Use do-notation!
apply (Func fmls body cenv) args | length fmls == length args =
    do  t_env <- get
        put cenv -- modify $ H.union cenv
        let temp_list = zip fmls args
        modify $ H.union (H.fromList temp_list)
        -- map (\(k,v) -> modify $ H.union k v) temp_list
        res <- eval body
        put t_env
        return res

-- Primitive
-- TODO: implement primitive function application
-- Since a primitive function has type `[Val] -> EvalState Val`, all you
-- need is to apply it to arguments
apply (PrimFunc p) args = p args
-- Other values are not applicable
-- TODO: you should simply throw a diagnostic
apply f args = throwError $ CannotApply f args