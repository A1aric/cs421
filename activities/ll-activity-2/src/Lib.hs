module Lib
    ( someFunc
    , Exp(..)
    , display
    , parse
    , parseE
    ) where

import Text.Regex.TDFA

data Exp = PlusExp Exp Exp
         | IntExp Integer
         | VarExp String
         | LetExp String Exp Exp
    deriving (Show,Eq)

someFunc :: IO ()
someFunc = putStrLn "someFunc"

display :: Exp -> String
display (VarExp v) = v
display (PlusExp e1 e2) = "+ " ++ display e1 ++ " " ++ display e2
display (LetExp v e1 e2) = "let " ++ v ++ " = " ++ display e1 ++ " in " ++ display e2 ++ " end"
display (IntExp i) = show i

isInt :: String -> Bool
isInt i = i =~ "[0-9]+"

isSymbol :: String -> String -> Bool
isSymbol s v = s == v

parseSymbol s (x:xs) =
  if s == x
     then (s,xs)
     else error $ "Parse error, expected " ++ s ++ " but got " ++ x ++ "."

-- let ie = ah in (let bex = tm in + (+ evj t) mir)
-- let ynbx = 0 in dn end
-- let gb = r in let lppu = 1 in 0 end end

-- a = ?? in ?? end
-- (tail tail xs) -> ?? in ?? end
--

-- Grammar
--
-- E -> + E E
--    | int
--    | var
--    | ( E )
--    | let var = E in E end

-- worked with mostofi2
parse xx = parseE (words xx)

parseE (x:xs)   | isInt x           = (IntExp (read x), xs)
                | isSymbol x "+"    = (PlusExp s1 s2, b)
                | isSymbol x "let"  = (LetExp (head xs) e1 e2, (tail r2))
                | isSymbol x "("    = (s1, (tail a))
                | otherwise         = (VarExp x, xs)
                where
                    (s1,a)  = parseE xs
                    (s2,b)  = parseE a
                    (e1,r1) = parseE (tail (tail xs))
                    (e2,r2) = parseE (tail r1)