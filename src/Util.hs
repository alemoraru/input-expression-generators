module Util ( someFunc, Error ( .. ) ) where

data Error = TypeError String
           | InterpError String 
           deriving (Eq)

instance Show Error where
  show (TypeError err)   = "Type Error: " ++ err
  show (InterpError err) = "Interp Error: " ++ err
  
someFunc :: IO ()
someFunc = putStrLn "someFunc"
