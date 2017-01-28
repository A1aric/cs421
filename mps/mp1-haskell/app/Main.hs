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
app xx     []     = xx
app []     yy     = yy
app (x:xs) yy     = x : app xs yy

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
addpairs :: (Num a) => [a] -> [a] -> [a]
addpairs (xx) (yy)  = aux $ (myzip xx yy)
        where
            aux []         = []
            aux ((a,b):xs) = (a + b) : aux xs


--- ### ones

-- don't forget to put the type declaration or you will lose points!
ones :: [Integer]
ones = 1: map (+ 0) ones

--- ### nats

-- don't forget to put the type declaration or you will lose points!
nats :: [Integer]
nats = 0 : map (+ 1) nats

--- ### fib

-- don't forget to put the type declaration or you will lose points!
fib :: [Integer]
fib = 0 : 1 : addpairs (fib) (tail fib)

--- Set Theory
--- ----------

--- ### add

-- don't forget to put the type declaration or you will lose points!
add :: Ord a => a -> [a] -> [a]
add val []      = [val]
add val (x:xs)
    | x > val   = val : x : xs
    | x == val  = x : xs
    | otherwise = x : add val (xs)

--- ### union

-- don't forget to put the type declaration or you will lose points!
union :: Ord a => [a] -> [a] -> [a]
union xx [] = xx
union [] yy = yy
union (x:xs) (y:ys)
    | x < y     = x : union xs (y:ys)
    | y < x     = y : union (x:xs) ys
    | otherwise = x : (union xs ys)

--- ### intersect

-- don't forget to put the type declaration or you will lose points!
intersect :: Ord a => [a] -> [a] -> [a]
intersect xx     []     = []
intersect []     yy     = []
intersect (x:xs) (y:ys)
    | x < y     = intersect xs (y:ys)
    | y < x     = intersect (x:xs) ys
    | otherwise = x : (intersect xs ys)

--- ### powerset

-- don't forget to put the type declaration or you will lose points!
powerset :: Ord a => [a] -> [[a]]
powerset []     = [[]]
powerset (x:xs) = union (powerset xs) (map (x:) (powerset xs))

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
list2cons :: [a] -> List a
list2cons []        = Nil
list2cons (x:xs)    = Cons x (list2cons xs)

--- ### cons2list

-- don't forget to put the type declaration or you will lose points!
cons2list :: List a -> [a]
cons2list Nil           = []
cons2list (Cons x (xs)) = x : cons2list xs

--- ### eval

-- don't forget to put the type declaration or you will lose points!
eval :: Exp -> Integer
eval = undefined

--- ### list2cons'

-- don't forget to put the type declaration or you will lose points!
list2cons' :: [a] -> List a
list2cons' (x:xs) = undefined--foldr (idk) Nil

--- ### BinTree

-- BinTree

--- ### sumTree

-- don't forget to put the type declaration or you will lose points!
--sumTree :: Num a => BinTree a -> a
sumTree = undefined

--- ### SimpVal

-- SimpVal

--- ### liftIntOp

-- don't forget to put the type declaration or you will lose points!
--liftIntOp :: (Integer -> Integer -> Integer) -> SimpVal -> SimpVal -> SimpVal
liftIntOp = undefined
