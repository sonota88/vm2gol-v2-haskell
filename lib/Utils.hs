module Utils where

isDigitChar :: Char -> Bool
isDigitChar c = ('0' <= c) && (c <= '9')

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
