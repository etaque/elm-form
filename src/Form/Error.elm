module Form.Error (Error(..)) where

type Error e
  = EmptyError
  | InvalidString
  | InvalidInt
  | InvalidFloat
  | InvalidBool
  | InvalidDate
  | SmallerThan Int
  | GreaterThan Int
  | ShorterThan Int
  | LongerThan Int
  | CustomError e
