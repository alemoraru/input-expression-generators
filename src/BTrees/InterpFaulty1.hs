module BTrees.InterpFaulty1 where

import BTrees.Grammar ( Tree(..) )

interp :: Tree Int -> Int
interp (Leaf x)       = x
interp (Branch l x r) = interp l + x + interp l