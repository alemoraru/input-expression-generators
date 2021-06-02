module Booleans.Interp1 where
  
import Booleans.Grammar ( Expr(..) )

-- Correct interpreter for boolean expressions
interp :: Expr -> Bool
interp (Val x)          = x
interp (And left right) = interp left && interp right
interp (Or left right)  = interp left || interp right
interp (Not expr)       = not $ interp expr