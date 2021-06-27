-- import Types
import qualified Json as Json

-- -- test 01
-- main :: IO ()
-- main = do
--   putStr (Json.toPrettyJson [])

-- -- test 02
-- main :: IO ()
-- main = do
--   putStr (Json.toPrettyJson [IntNode 1])

-- -- test 03
-- main :: IO ()
-- main = do
--   putStr (Json.toPrettyJson [StrNode "fdsa"])

-- -- test 04
-- main :: IO ()
-- main = do
--   putStr (Json.toPrettyJson [IntNode (-123)])

-- -- test 05
-- main :: IO ()
-- main = do
--   putStr (Json.toPrettyJson [IntNode 123, StrNode "fdsa"])

-- -- test 06
-- main :: IO ()
-- main = do
--   putStr (Json.toPrettyJson [ListNode []])

-- -- test 07
-- main :: IO ()
-- main = do
--   putStr (Json.toPrettyJson [
--              IntNode 1,
--              StrNode "a",
--              ListNode [
--                  IntNode 2,
--                  StrNode "b"
--                  ],
--              IntNode 3,
--              StrNode "c"
--              ]
--          )

-- -- test 08
-- main :: IO ()
-- main = do
--   putStr (Json.toPrettyJson [StrNode "漢字"])

main :: IO ()
main = do
  input <- getContents
  putStr (Json.toPrettyJson (Json.parse input))
