module STLC.Interp where

import STLC.Grammar ( Expr(..), Environment, Val )

-- Interpreter for STLC
interp :: Environment -> Expr -> Val 
interp env (Var i) = undefined 
interp env (App x) = undefined 
interp env (Lam x) = undefined 
