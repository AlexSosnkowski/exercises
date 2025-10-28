{- |
Module          : Lecture1
Copyright         : (c) 2021-2022 Haskell Beginners 2022 Course
SPDX-License-Identifier : MPL-2.0
Maintainer        : Haskell Beginners 2022 Course <haskell.beginners2022@gmail.com>
Stability         : Stable
Portability       : Portable

Exercises for the Lecture 1 of the Haskell Beginners course.

To complete exercises, you need to complete implementation and add
missing top-level type signatures. You can implement any additional
helper functions. But you can't change the names of the given
functions.

Comments before each function contain explanations and example of
arguments and expected returned values.

It's absolutely okay if you feel that your implementations are not
perfect. You can return to these exercises after future lectures and
improve your solutions if you see any possible improvements.
-}

module Lecture1
  ( makeSnippet
  , sumOfSquares
  , lastDigit
  , minmax
  , subString
  , strSum
  , lowerAndGreater
  ) where
import Control.Exception (ErrorCall(ErrorCall))
import Control.Monad (when)

-- VVV If you need to import libraries, do it after this line ... VVV

-- ^^^ and before this line. Otherwise the test suite might fail  ^^^

{- | Specify the type signature of the following function. Think about
its behaviour, possible types for the function arguments and write the
type signature explicitly.
-}
makeSnippet:: Int -> [Char] -> [Char] 
makeSnippet limit text = take limit ("Description: " ++ text) ++ "..."

{- | Implement a function that takes two numbers and finds sum of
their squares.

>>> sumOfSquares 3 4
25

>>> sumOfSquares (-2) 7
53

Explanation: @sumOfSquares 3 4@ should be equal to @9 + 16@ and this
is 25.
-}
-- DON'T FORGET TO SPECIFY THE TYPE IN HERE
sumOfSquares :: Int -> Int -> Int 
sumOfSquares x y = x*x + y*y 

{- | Implement a function that returns the last digit of a given number.

>>> lastDigit 42
2
>>> lastDigit (-17)
7

🕯 HINT: use the @mod@ function

-}
-- DON'T FORGET TO SPECIFY THE TYPE IN HERE
--flip sign if negative to get last digit
-- I wonder if I can do this "branchless"
lastDigit :: Int -> Int 
lastDigit n
  | n > 0 = mod n 10
  | otherwise = mod (-n) 10  
{- | Write a function that takes three numbers and returns the
difference between the biggest number and the smallest one.

>>> minmax 7 1 4
6

Explanation: @minmax 7 1 4@ returns 6 because 7 is the biggest number
and 1 is the smallest, and 7 - 1 = 6.

Try to use local variables (either let-in or where) to implement this
function.
-}
--this feels like wasted computation because we should be able to rule out the min from the max search 
-- also limited to Ints, can be made to work more generally I think 
-- the method below seems inefficent, but looks nicer 
minmax::Int -> Int -> Int -> Int
minmax x y z = 
  let 
    minv = min x (min y z)
    maxv = max x (max y z)
  in 
    maxv - minv

-- the below method is ugly, but it feels more efficent because I traverse only once 
-- (but there are more condition checks so barely any better really), 
-- maybe I should split the go into a dedicated function?   
-- minmax x y z = go [x, y, z] x x 
--   where 
--     go :: [Int] -> Int -> Int -> Int 
--     go t_l t_min t_max | null t_l = t_max - t_min    
--       | head t_l < t_min = go (tail t_l) (head t_l) t_max 
--       | head t_l > t_max = go (tail t_l) t_min (head t_l) 
--       | otherwise = go (tail t_l) t_min t_max 

  



{- | Implement a function that takes a string, start and end positions
and returns a substring of a given string from the start position to
the end (including).

>>> subString 3 7 "Hello, world!"
"lo, w"

>>> subString 10 5 "Some very long String"
""

This function can accept negative start and end position. Negative
start position can be considered as zero (e.g. substring from the
first character) and negative end position should result in an empty
string.
-}
--the method below may look more elegant, but I fear efficency 
subString :: Int -> Int -> [Char] -> [Char]
subString start end str 
  | end < 0 = []
  | start < 0 = take (end+1) str
  | otherwise = drop start (take (end+1) str)

-- recursive method, I think it is more efficent since reversal is linear time 
-- subString start end str 
--   |  end >= 0 = go start (take (end+1) str) -- inclusive substrings lol 
--   | otherwise = []
--   where 
--     go::Int -> [Char] -> [Char]
--     go a b 
--       | null b = []
--       | a > 0 = go (a-1) (tail b)  
--       | otherwise = b

{- | Write a function that takes a String — space separated numbers,
and finds a sum of the numbers inside this string.

>>> strSum "100  -42  15"
73

The string contains only spaces and/or numbers.
-}
strSum :: [Char] -> Int
strSum str = 
  let 
    nums = words str
  in 
    sum (map (read :: [Char] -> Int) nums) 
-- I know its cheating to use map before its covered in lecture, but it seemed fitting

-- naive method below
-- strSum str = go (words str) 0
--   where 
--     go::[[Char]] -> Int -> Int 
--     go nums total
--       | null nums = total
--       | otherwise = go (tail nums) (total + read (head nums))
        
      

{- | Write a function that takes a number and a list of numbers and
returns a string, saying how many elements of the list are strictly
greater than the given number and strictly lower.

>>> lowerAndGreater 3 [1 .. 9]
"3 is greater than 2 elements and lower than 6 elements"

Explanation: the list [1 .. 9] contains 9 elements: [1, 2, 3, 4, 5, 6, 7, 8, 9]
The given number 3 is greater than 2 elements (1 and 2)
and lower than 6 elements (4, 5, 6, 7, 8 and 9).

🕯 HINT: Use recursion to implement this function.
-}
lowerAndGreater::Int -> [Int] -> [Char]
lowerAndGreater n list = go list 0 0
  where 
    go::[Int] -> Int -> Int -> [Char]
    go n_list low high
      | null n_list = show n ++ " is greater than " ++ show  high ++ " elements and lower than " ++ show low ++ " elements"
      | n > head n_list = go (tail n_list) low (high + 1)
      | n < head n_list = go (tail n_list) (low+1) high
      | otherwise = go (tail n_list) low high
