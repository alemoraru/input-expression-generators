module Booleans.InterpFaulty3 where

import Booleans.Grammar ( Expr(..) )

-- Faulty interpreter for boolean expressions
interp :: Expr -> Bool
interp (Val x)             = x
interp (And (left, right)) = interp left && interp right
interp (Or  (left, right)) = interp left || interp right
interp (Not expr)          = interp expr -- introduced flaw here