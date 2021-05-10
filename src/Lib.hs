module Lib ( someFunc, Error ( .. ) ) where

-- not newtype because we might add unsafe head and tail once we try a grammar with lists
data Error = TypeError String
           | InterpError String 
           deriving (Eq)

instance Show Error where
  show (TypeError err)   = "Type Error: " ++ err
  show (InterpError err) = "Interp Error: " ++ err
  
someFunc :: IO ()
someFunc = putStrLn "someFunc"
