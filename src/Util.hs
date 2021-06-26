module Util ( Error ( .. ) ) where

-- ADT for reporting errors
data Error = TypeError String
           | InterpError String 
           deriving (Eq)

-- Used for pretty printing errors
instance Show Error where
  show (TypeError err)   = "Type Error: " ++ err
  show (InterpError err) = "Interp Error: " ++ err
