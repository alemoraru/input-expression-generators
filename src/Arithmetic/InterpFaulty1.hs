module Arithmetic.InterpFaulty1 where
  
import Arithmetic.Grammar

interp :: Expr -> Int
interp (Val x) = x
interp (Add left right) = interp left + interp left
interp (Sub left right) = interp left - interp right
interp (Mul left right) = interp left * interp right
interp (Div left right) = interp left `div` interp right