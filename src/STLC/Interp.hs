module STLC.Interp where

import STLC.Grammar ( Expr(..), Environment, Val )
import Util ( Error(InterpError) )

-- Interpreter for STLC
interp :: Expr -> Environment -> Either Error Val
interp (Var i) env              = 
    if length env <= i 
        then Left  $ InterpError  $ "No binding found for variable " ++ show i ++ "." 
        else Right $ env !! i
interp (Lam ((i, t), body)) env = undefined 
interp (App (lam, arg)) env     = undefined 
