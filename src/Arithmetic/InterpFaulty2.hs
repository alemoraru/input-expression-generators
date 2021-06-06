module Arithmetic.InterpFaulty2 where

import Arithmetic.Grammar ( Expr(..) )

-- Faulty interpreter for arithmetic expressions
interp :: Expr -> Either String Int
interp (Val x)          = Right x
interp (Add left right) = 
  case interp left of
      Left err   -> Left err
      Right valL ->
           case interp right of
               Left err   -> Left err
               Right valR -> Right (valL + valR)
          
interp (Sub left right) = 
  case interp left of
      Left err   -> Left err
      Right valL ->
           case interp left of -- intrpduced error here
               Left err   -> Left err
               Right valR -> Right (valL - valR)

interp (Mul left right) = 
  case interp left of
      Left err   -> Left err
      Right valL ->
           case interp right of
               Left err   -> Left err
               Right valR -> Right (valL * valR)

interp (Div left right) = 
  case interp left of
      Left err   -> Left err
      Right valL ->
           case interp right of
               Left err   -> Left err
               Right valR -> if valR == 0 
                   then Left "Cannot divide by zero" 
                   else Right (valL `div` valR)