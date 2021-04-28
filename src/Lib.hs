module Lib where

-- not newtype because we might add unsafe head and tail once we try a grammar with lists
data Error = TypeError String
  | MissingIdError String

instance Show Error where
  show (TypeError err) = "Type Error: " ++ err
  show (MissingIdError err) = "Missing Id Error: " ++ err
  
someFunc :: IO ()
someFunc = putStrLn "someFunc"
