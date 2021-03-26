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
    ) where

{-|
  The 'up' function moves an element 'n' positions to 
  the beginning of a list. If the index provided is out 
  of range, the list is returned without any modification.
  On the other hand, extra steps will be ignored.

  Some examples are given below:

  >>> up 2 3 ["one", "two", "three"]
  ["three", "one", "two"]

  >>> up 4 1 ["one", "two", "three"]
  ["one", "two", "three"]

  >>> up 2 3 ["one", "two", "three"]
  ["three", "one", "two"]
-}
up :: Int -> Int -> [a] -> [a]
up index steps items
  | steps <= 0 || index <= 0 || index >= length items = items
  | otherwise = up prev (steps -1) oneStepUpList
  where
    oneStepUpList = take prev items ++ (items !! index) : items !! prev : drop next items
    prev = index - 1
    next = index + 1

{-|
  The 'down' function moves an element 'n' positions to 
  the end of a list. If the index provided is out of range,
  the list is returned without any modification. On the other 
  hand, extra steps will be ignored.

  Some examples are given below:

  >>> down 0 1 ["one", "two", "three"]
  ["two", "one", "three"]

  >>> up 4 1 ["one", "two", "three"]
  ["one", "two", "three"]

  >>> down 0 4 ["one", "two", "three"]
  ["two", "three", "one"]
-}
down :: Int -> Int -> [a] -> [a]
down index steps items = reverse . up reverseIndex steps $ reverse items
  where
    reverseIndex = length items - index - 1
