--- Getting Started
--- ===============

--- Relevant Files
--- --------------

module Main where

main :: IO ()
main = return ()

--- Problems
--- ========

--- Recursion
--- ---------

--- ### mytake

-- don't forget to put the type declaration or you will lose points!
mytake :: Int -> [a] -> [a]
mytake _   []       = []
mytake num (x:xs)   =   if num > 0
                        then x : mytake (num-1) (xs)
                        else []

--- ### mydrop

-- don't forget to put the type declaration or you will lose points!
mydrop :: Int -> [a] -> [a]
mydrop _   []       = []
mydrop num (x:xs)   =   if num <= 0
                        then x:xs
                        else mydrop (num-1) xs

--- ### rev

-- don't forget to put the type declaration or you will lose points!
rev :: [a] -> [a]
rev []      = []
rev (x:xs)  = revHelper (x:xs) []
    where
        revHelper []     temp = temp
        revHelper (x:xs) temp = revHelper xs (x:temp)

--- ### app

-- don't forget to put the type declaration or you will lose points!
app :: [a] -> [a] -> [a]
app (x:xs) []     = (x:xs)
app []     (y:ys) = (y:ys)
app (x:xs) (y:ys) = []

--- ### inclist

-- don't forget to put the type declaration or you will lose points!
inclist :: Num a => [a] -> [a]
inclist []      = []
inclist (x:xs)  = (1 + x) : inclist xs

--- ### sumlist

-- don't forget to put the type declaration or you will lose points!
sumlist :: Num a => [a] -> a
sumlist []      = 0
sumlist (x:xs)  = x + sumlist xs

--- ### myzip

-- don't forget to put the type declaration or you will lose points!
myzip :: [a] -> [b] -> [(a,b)]
myzip _      []     = []
myzip []     _      = []
myzip (x:xs) (y:ys) = (x,y) : myzip xs ys

--- ### addpairs

-- don't forget to put the type declaration or you will lose points!
addpairs = undefined

--- ### ones

-- don't forget to put the type declaration or you will lose points!
ones = undefined

--- ### nats

-- don't forget to put the type declaration or you will lose points!
nats = undefined

--- ### fib

-- don't forget to put the type declaration or you will lose points!
fib = undefined

--- Set Theory
--- ----------

--- ### add

-- don't forget to put the type declaration or you will lose points!
add = undefined

--- ### union

-- don't forget to put the type declaration or you will lose points!
union = undefined

--- ### intersect

-- don't forget to put the type declaration or you will lose points!
intersect = undefined

--- ### powerset

-- don't forget to put the type declaration or you will lose points!
powerset = undefined

--- Higher Order Functions
--- ----------------------

--- ### inclist'

-- don't forget to put the type declaration or you will lose points!
inclist' :: Num a => [a] -> [a]
inclist' (xx) = map (+1) xx

--- ### sumlist'

-- don't forget to put the type declaration or you will lose points!
sumlist' :: (Num a) => [a] -> a
sumlist' (xx) = foldr (+) 0 xx

--- Algebraic Data Types
--- --------------------

data List a = Cons a (List a)
            | Nil
  deriving (Show, Eq)

data Exp = IntExp Integer
         | PlusExp [Exp]
         | MultExp [Exp]
  deriving (Show, Eq)

--- ### list2cons

-- don't forget to put the type declaration or you will lose points!
list2cons = undefined

--- ### cons2list

-- don't forget to put the type declaration or you will lose points!
cons2list = undefined

--- ### eval

-- don't forget to put the type declaration or you will lose points!
eval = undefined

--- ### list2cons'

-- don't forget to put the type declaration or you will lose points!
list2cons' = undefined

--- ### BinTree

-- BinTree

--- ### sumTree

-- don't forget to put the type declaration or you will lose points!
sumTree = undefined

--- ### SimpVal

-- SimpVal

--- ### liftIntOp

-- don't forget to put the type declaration or you will lose points!
--liftIntOp :: (Integer -> Integer -> Integer) -> SimpVal -> SimpVal -> SimpVal
liftIntOp = undefined
