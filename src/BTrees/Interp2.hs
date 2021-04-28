module BTrees.Interp2 where

import BTrees.Grammar

interp :: Tree Int -> Int
interp (Leaf x)     = x
interp (Branch l r) = interp r + interp l