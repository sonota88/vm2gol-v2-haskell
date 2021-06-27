import Types
import qualified Utils as U
import qualified Json as Json

main :: IO ()
main = do
  src <- getContents
  putStr $ Main.lex src

tokenToStr :: Int -> String -> String -> String
tokenToStr lineno kind val =
  Json.toJson [
    IntNode lineno,
    StrNode kind,
    StrNode val
  ]

isIdentChar :: Char -> Bool
isIdentChar c =
  ('a' <= c && c <= 'z')
  || U.isDigitChar c
  || c == '_'

matchIdent :: String -> Int
matchIdent rest =
  let iter _rest i =
        if isIdentChar (head _rest) then
          iter (tail _rest) (i + 1)
        else
          i
  in
    iter rest 0

matchSym :: String -> Int
matchSym rest =
  case rest of
    '=' : '=' : _ -> 2
    '!' : '=' : _ -> 2
    _ ->
      let c = (head rest)
      in
        if (c == '(')
         || (c == ')')
         || (c == '{')
         || (c == '}')
         || (c == ';')
         || (c == '=')
         || (c == ',')
         || (c == '+')
         || (c == '*')
        then
          1
        else
          0

charIndex :: String -> Char -> Maybe Int
charIndex str targetChar =
  let iter s tc i =
        case s of
          "" -> Nothing
          c : _ ->
            if c == tc then
              Just i
            else
              iter (tail s) tc (i + 1)
  in
    iter str targetChar 0

matchCmt :: String -> Int
matchCmt rest =
  if length rest < 2 then
    0
  else
    if (take 2 rest) == "//" then
      case charIndex rest '\n' of
        Just i -> i
        Nothing -> error "matchCmt: not supported"
    else
      0

isKw :: String -> Bool
isKw s =
  (s == "func")
  || (s == "var")
  || (s == "set")
  || (s == "call")
  || (s == "return")
  || (s == "call_set")
  || (s == "while")
  || (s == "case")

iterLex :: String -> Int -> String
iterLex rest lineno =
  case rest of
    "" -> ""
    c : rest2 ->
      case c of
        ' ' -> iterLex rest2 lineno
        '\n' -> iterLex rest2 (lineno + 1)
        '-' ->
          let size = (U.matchInt rest2) + 1
              val = (take size rest)
          in
            (tokenToStr lineno "int" val) ++ "\n"
            ++ (iterLex (drop size rest) lineno)
        '/' | 0 < (matchCmt rest) ->
            let size = matchCmt rest
            in
              iterLex (drop size rest) lineno
        '"' | 0 < (U.matchStr rest2) ->
            let size = U.matchStr rest2
                val = (take size rest2)
            in
              (tokenToStr lineno "str" val) ++ "\n"
              ++ iterLex (drop (size + 2) rest) lineno
        _ | 0 < (matchSym rest) ->
            let size = matchSym rest
                val = (take size rest)
            in
              (tokenToStr lineno "sym" val) ++ "\n"
              ++ (iterLex (drop size rest) lineno)
          | 0 < (U.matchInt rest) ->
            let size = (U.matchInt rest)
                val = (take size rest)
            in
              (tokenToStr lineno "int" val) ++ "\n"
              ++ (iterLex (drop size rest) lineno)
          | 0 < (matchIdent rest) ->
            let size = matchIdent rest
                val = (take size rest)
                kind = (if isKw val then "kw" else "ident")
            in
              (tokenToStr lineno kind val) ++ "\n"
              ++ (iterLex (drop size rest) lineno)
          | otherwise -> error (
              "unexpected pattern (" ++ (show rest) ++ ")"
                ++ " at line " ++ (show lineno)
              )

lex :: String -> String
lex src =
  iterLex src 1
