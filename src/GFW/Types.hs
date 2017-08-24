module GFW.Types where

data Field
  = Comment String
  | AutoProxy String
  | URL String
  | Domain String
  | IP Address
       Port
  | WhiteList String
  | RegExp String
  | Invalid String
  deriving (Eq, Show)

type Address = [Int]

type Port = Int
