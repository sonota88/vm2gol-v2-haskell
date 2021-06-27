module Types where

data TNode =
  IntNode Int
  | StrNode String
  | ListNode [TNode]
  deriving Show
