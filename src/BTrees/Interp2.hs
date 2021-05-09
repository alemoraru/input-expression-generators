module BTrees.Interp2 where

import BTrees.Grammar ( Tree(..) )

interp :: Tree Int -> Int
interp (Leaf x)       = x
interp (Branch l x r) = interp r + x + interp l