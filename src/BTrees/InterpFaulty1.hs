module BTrees.InterpFaulty1 where

import BTrees.Grammar

interp :: Tree Int -> Int
interp (Leaf x)     = x
interp (Branch l r) = interp l + interp l