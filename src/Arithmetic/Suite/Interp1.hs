module Arithmetic.Suite.Interp1 where
  
import Arithmetic.Grammar ( Expr(..) )

-- Correct interpreter for arithmetic expressions
-- That either returns an error message or an Int result
interp :: Expr -> Either String Int
interp (Val x)             = Right x
interp (Add (left, right)) = 
  case interp left of
      Left err   -> Left err
      Right valL ->
           case interp right of
               Left err   -> Left err
               Right valR -> Right (valL + valR)
          
interp (Sub (left, right)) = 
  case interp left of
      Left err   -> Left err
      Right valL ->
           case interp right of
               Left err   -> Left err
               Right valR -> Right (valL - valR)

interp (Mul (left, right)) = 
  case interp left of
      Left err   -> Left err
      Right valL ->
           case interp right of
               Left err   -> Left err
               Right valR -> Right (valL * valR)

interp (Div (left, right)) = 
  case interp left of
      Left err   -> Left err
      Right valL ->
           case interp right of
               Left err   -> Left err
               Right valR -> if valR == 0 
                   then Left "Cannot divide by zero" 
                   else Right (valL `div` valR)