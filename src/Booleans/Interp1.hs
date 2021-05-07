module Booleans.Interp1 where
  
import Booleans.Grammar

interp :: Expr -> Bool
interp (Box x)          = x
interp (And left right) = interp left && interp right
interp (Or left right)  = interp left || interp right
interp (Not expr)       = not $ interp expr