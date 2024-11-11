module Check.AST.Indent.FuncRhs where

import Curry.SpanInfo
import Curry.Span
import Curry.Position
import Curry.Types
import Text.Pretty
import Control.Monad ( unless )

import Types

-- Applies actual check on function declarations.
checkRhs :: Decl a -> Int -> CSM ()
checkRhs e _ =
  case e of
    (FunctionDecl sI _ _ eqs) -> checkRhs' sI eqs
    _                         -> return ()

-- Gets `rhs` of equations and checks the alignment of their root (either guards or equal
-- signs), the alignement within an rhs is checked by their respective checks
-- however.
checkRhs' :: SpanInfo -> [Equation a] -> CSM ()
checkRhs' (SpanInfo sp _) eqs = do
  let (rhs:rhss) = map getRhs eqs
  unless (checkAlign getCol (getCol (getSpanInfo rhs)) rhss)
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
checkRhs' NoSpanInfo _ = return ()

-- Returns right-hand side of a equation.
getRhs :: Equation a -> Rhs a
getRhs (Equation _ _ _ rhs) = rhs