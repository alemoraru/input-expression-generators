module BTrees.Interp1 where

import BTrees.Grammar

interp :: Tree Int -> Int
interp (Leaf x)     = x
interp (Branch l r) = interp l + interp r