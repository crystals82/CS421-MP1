--- Getting Started
--- ===============

--- Relevant Files
--- --------------

module Lib where

-- This line imports the Prelude module without certain functions
import Prelude hiding ( take, drop, reverse
                      , zip, zipWith
                      , map, foldl, foldr
                      , iterate, repeat
                      , replicate, cycle
                      , (++)
                      )
-- When you are allowed to use builtin functions Prepend them with "P."
-- for example `P.take`
import qualified Prelude as P

--- Problems
--- ========

--- Recursion
--- ---------

--- ### mytake

-- don't forget to put the type declaration or you will lose points!
mytake :: Int -> [a] -> [a]
mytake a [] = []
mytake a (x:xs) | a <= 0 = []
                | otherwise =  x:(mytake (a-1) xs)

--- ### mydrop

-- don't forget to put the type declaration or you will lose points!
mydrop :: Int -> [a] -> [a]
mydrop a [] = []
mydrop a (x:xs) | a <= 0 = (x:xs)
                | otherwise =  (mydrop (a-1) xs)

--- ### rev

-- don't forget to put the type declaration or you will lose points!
rev_helper :: [a] -> [a] -> [a]
rev_helper retval [] = retval
rev_helper retval (y:ys) = rev_helper (y:retval) ys

rev :: [a] -> [a]
rev [] = []
rev (x:xs) = rev_helper [] (x:xs)

--- ### app

-- don't forget to put the type declaration or you will lose points!
app :: [a] -> [a] -> [a]
app [] [] = []
app x [] = x
app [] y = y
app (x:xs) y = x:(app xs y)

--- ### inclist

-- don't forget to put the type declaration or you will lose points!
inclist :: Num a => [a] -> [a]
inclist [] = [] --base case first
inclist (x:xs) = x + 1 : inclist (xs)

--- ### sumlist

-- don't forget to put the type declaration or you will lose points!
sumlist :: Num a => [a] -> a
sumlist [] = 0
sumlist (x:xs) = x + sumlist xs

--- ### myzip

-- don't forget to put the type declaration or you will lose points!
myzip :: [a] -> [b] -> [(a, b)]
myzip [] [] = []
myzip x [] = []
myzip [] y = []

myzip (x:xs) (y:ys) = ( (x, y)  : myzip (xs) (ys) )

--- ### addpairs

-- don't forget to put the type declaration or you will lose points!
myzip_help :: [a] -> [a] -> [(a, a)]
myzip_help [] [] = []
myzip_help x [] = []
myzip_help [] y = []
myzip_help (x:xs) (y:ys) = ( (x, y)  : myzip_help (xs) (ys) )

actualadd :: (Num a) => [(a, a)] -> [a]
actualadd [] = []
actualadd (x:xs) = ( fst(x) + snd(x) : actualadd (xs) ) 

addpairs :: (Num a) => [a] -> [a] -> [a]
addpairs [] [] = []
addpairs x [] = []
addpairs [] y = []
addpairs (x:xs) (y:ys) = actualadd(myzip_help (x:xs) (y:ys))

--- ### ones

-- don't forget to put the type declaration or you will lose points!
ones :: [Integer]
ones = 1:ones

--- ### nats

-- don't forget to put the type declaration or you will lose points!
nats :: [Integer]
nats = 0:inclist(nats)

--- ### fib

-- don't forget to put the type declaration or you will lose points!

fib :: [Integer]
fib = 0:1:1:addpairs (tail(fib)) ( tail(tail(fib)) )

--- Set Theory
--- ----------

--- ### add

-- don't forget to put the type declaration or you will lose points!
add :: Ord a => a -> [a] -> [a]
add x [] = [x]
add x (y:ys) | x < y = x:y:ys
             | x == y = x:ys
             | otherwise = y:add x ys

--- ### union

-- don't forget to put the type declaration or you will lose points!
union :: Ord a => [a] -> [a] -> [a]
union [] [] = []
union x [] = x
union [] y = y 
union (x:xs) (y:ys) | x < y = x:union (xs) (y:ys)
                    | x == y = x:union (xs) (ys)
                    | otherwise = y:union (x:xs) (ys) 

--- ### intersect

-- don't forget to put the type declaration or you will lose points!
intersect :: Ord a => [a] -> [a] -> [a]
intersect [] [] = []
intersect x [] = []
intersect [] y = []
intersect (x:xs) (y:ys)
    | x == y = x:intersect (xs) (ys)
    | x < y = intersect (xs) (y:ys)
    | y < x = intersect (x:xs) (ys)

--- ### powerset

-- don't forget to put the type declaration or you will lose points!
powerset :: Ord a => [a] -> [[a]]
powerset [] = [[]]
powerset (x:xs) =
  let p = powerset xs
    in union p [add x s | s <- p]

--- Higher Order Functions
--- ----------------------

--- ### inclist'

-- don't forget to put the type declaration or you will lose points!
inc x = x+1

inclist' :: Num a => [a] -> [a]
inclist' = P.map inc

--- ### sumlist'

-- don't forget to put the type declaration or you will lose points!
plus a b = a+b

sumlist' :: (Num a) => [a] -> a
sumlist' = P.foldr (\a b -> plus a b) 0
