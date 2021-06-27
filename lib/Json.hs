module Json where

import Data.List
import Types
import qualified Utils as U

indent :: Int -> String
indent lv = replicate (lv * 2) ' '

nodeToStr :: TNode -> Int -> Bool -> String
nodeToStr node lv pretty =
  case node of
    IntNode n -> (if pretty then (indent lv) else "") ++ (show n)
    StrNode s -> (if pretty then (indent lv) else "") ++ "\"" ++ s ++ "\""
    ListNode els -> listToStr els lv pretty

listToStr :: [TNode] -> Int -> Bool -> String
listToStr nodes lv pretty =
  (if pretty then (indent lv) else "")
  ++ "[" ++ (if pretty then "\n" else "")
  ++ (
      (intercalate
        (if pretty then ",\n" else ", ")
        (map (\node -> nodeToStr node (lv + 1) pretty) nodes))
     )
  ++ (if pretty then "\n" else "")
  ++ (if pretty then (indent lv) else "") ++ "]"

toPrettyJson :: [TNode] -> String
toPrettyJson nodes = (listToStr nodes 0 True)

toJson :: [TNode] -> String
toJson nodes = listToStr nodes 0 False

parseList :: String -> ([TNode], String)
parseList rest =
  case rest of
    "" -> ([], "")
    hd : tl ->
      case hd of
        ']' -> ([], tl)
        '[' ->
          let (xs, tl2) = parseList tl
              (els_after_xs, tl3) = parseList tl2
          in
            ((ListNode xs : els_after_xs), tl3)
        ' ' -> parseList tl
        ',' -> parseList tl
        '\n' -> parseList tl
        _ | U.isDigitChar hd ->
          let size = U.matchInt rest
              (nodes_after_int, tl2) = (parseList (drop size rest))
          in
            ((IntNode (read (take size rest)) : nodes_after_int), tl2)
        '-' ->
          let size = (U.matchInt tl) + 1
              (nodes_after_int, tl2) = (parseList (drop size rest))
          in
            ((IntNode (read (take size rest)) : nodes_after_int), tl2)
        '"' ->
          let size = U.matchStr tl
              (nodes_after_str, tl2) = (parseList (drop (size + 2) rest))
          in
            ((StrNode (take size (tail rest)) : nodes_after_str), tl2)
        _ -> error ("unexpected pattern (" ++ (show rest) ++ ")")

parse :: String -> [TNode]
parse json =
  let (nodes, _) = parseList (tail json)
  in
    nodes
