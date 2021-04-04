{-|
Module      : Data.List.Scroll
Description : Reordering an item within a list
Copyright   : (c) Felpe Santa-Cruz, 2021
License     : BSD3
Maintainer  : fesanmar@gmail.com
Stability   : stable
Portability : POSIX

This module provides functions for
relocate an item within a list.
-}
module Data.List.Scroll
    ( up
    , down
    , deleteByIndex
    ) where

import Data.Tuple.Extra ( second )

{-|
  The 'up' function moves an element 'n' positions to 
  the beginning of a list. If the index provided is out 
  of range, the list is returned without any modification.
  On the other hand, extra steps will be ignored.

  Some examples are given below:

  >>> up 2 2 ["one", "two", "three"]
  ["three", "one", "two"]

  >>> up 4 1 ["one", "two", "three"]
  ["one", "two", "three"]

  >>> up 2 3 ["one", "two", "three"]
  ["three", "one", "two"]
-}
up :: Int -> Int -> [a] -> [a]
up index steps ls
  | any (<= 0) [steps, index, length ls - index] = ls
  | otherwise = take prev ls ++ (ls !! index) : tailNoTarget
  where prev = index - steps
        tailNoTarget = drop prev $ deleteByIndex index ls

{-|
  The 'down' function moves an element 'n' positions to 
  the end of a list. If the index provided is out of range,
  the list is returned without any modification. On the other 
  hand, extra steps will be ignored.

  Some examples are given below:

  >>> down 0 1 ["one", "two", "three"]
  ["two", "one", "three"]

  >>> down 4 1 ["one", "two", "three"]
  ["one", "two", "three"]

  >>> down 0 4 ["one", "two", "three"]
  ["two", "three", "one"]
-}
down :: Int -> Int -> [a] -> [a]
down index steps ls = reverse . up reverseIndex steps $ reverse ls
  where reverseIndex = length ls - index - 1

{-|
  The 'deleteByIndex' function removes an element
  from a list by its position within it. For example,

  >>> deleteByIndex 1 ["one", "two", "three"]
  ["one", "three"]

  If index is out of range, the list will be returned
  with no changes. For example:

  >>> deleteByIndex 3 ["one", "two", "three"]
  ["one", "two", "three"]
-}
deleteByIndex :: Int -> [a] -> [a]
deleteByIndex index ls
  | index < 0 = ls
  | otherwise = uncurry (++) . second (drop 1) $ splitAt index ls