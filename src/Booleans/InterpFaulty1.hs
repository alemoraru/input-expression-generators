module Booleans.InterpFaulty1 where
  
import Booleans.Grammar ( Expr(..) )

-- Faulty interpreter for boolean expressions
interp :: Expr -> Bool
interp (Val x)          = x
interp (And left right) = interp left || interp right -- introduced a flaw here
interp (Or left right)  = interp left || interp right
interp (Not expr)       = not $ interp expr