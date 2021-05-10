module Booleans.Interp2 where
  
import Booleans.Grammar

interp :: Expr -> Bool
interp (Val x)          = x
interp (And left right) = interp right && interp left
interp (Or left right)  = interp right || interp left
interp (Not expr)       = not (interp expr)