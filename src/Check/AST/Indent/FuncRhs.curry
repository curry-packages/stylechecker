module Check.AST.Indent.FuncRhs where

import Curry.SpanInfo
import Curry.Span
import Curry.Position
import Curry.Types
import Text.Pretty

import Types

--applies actual check on functionDeclarations equations
checkRhs :: Decl a -> Int -> CSM ()
checkRhs e _ =
  case e of
    (FunctionDecl sI _ _ eqs) -> checkRhs' sI eqs
    _                         -> return ()

-- get rhs of equations and check the alignment of their root (either guards or equal
-- signs), the alignement within a rhs is checked by their respective checks
-- however
checkRhs' :: SpanInfo -> [Equation a] -> CSM ()
checkRhs' (SpanInfo sp _) eqs = do
  let (rhs:rhss) = map getRhs eqs
  unlessM (checkAlign getCol (getCol (getSpanInfo rhs)) rhss)
          (report (Message
                      sp
                      ( colorizeKey "guards"
                        <+> text "and"
                        <+> colorizeKey "equal signs"
                        <+> text "not aligned"
                      )
                      ( colorizeKey "guards"
                        <+> text "and"
                        <+> colorizeKey "equal signs"
                        <+> text "should be aligned in"
                        <+> colorizeKey "function declaration"
                      )
                )
            )

-- returns rhs of a equation
getRhs :: Equation a -> Rhs a
getRhs (Equation _ _ rhs) = rhs

