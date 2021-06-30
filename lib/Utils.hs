module Utils where

import Data.Char as Char

isDigitChar :: Char -> Bool
isDigitChar = Char.isDigit

matchInt :: String -> Int
matchInt str =
  let iter s n =
        if s == "" then
          n
        else
          if isDigitChar (head s) then
            iter (tail s) (n + 1)
          else
            n
  in
    iter str 0

matchStr :: String -> Int
matchStr str =
  let iter s n =
        if (head s) == '"' then
          n
        else
          iter (tail s) (n + 1)
  in
    iter str 0
