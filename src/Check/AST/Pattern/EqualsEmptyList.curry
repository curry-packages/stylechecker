module Check.AST.Pattern.EqualsEmptyList where

import Curry.SpanInfo
import Curry.Span
import Curry.Position
import Curry.Types
import Curry.Ident
import Text.Pretty

import Types

-- Check for infix operation and function with two parameters.
checkEqualsEmptyList :: Expression a -> Int -> CSM ()
checkEqualsEmptyList e _ = case e of
  (InfixApply
    _
    _
    (InfixOp _
      (QualIdent _ _
        (Ident _ _ _)))
    _)
    -> checkInfix e
  (Apply _
    (Apply _
      (Variable _ _
        (QualIdent _ _
          (Ident _ _ _)
        )
      )
      _)
    _)
    -> checkApply e
  _ -> return ()

-- Checks whether an infix comparison with the empty list (such as `x == []`) 
-- is used. If so, a warning is emitted (use 'null' instead).
checkInfix :: Expression a -> CSM ()
checkInfix e =
  case checkInfixCompare e of
    (sI, _, "==", (List _ _ []))
      -> report (Message
                  (getSpan sI)
                  (text "Do not use" <+> colorizeKey "a == []")
                  (text "Use" <+> colorizeKey "null a" <+> text "instead"))
    (sI, (List _ _ []), "==",  _)
      -> report (Message
                  (getSpan sI)
                  (text "Do not use" <+> colorizeKey "[] == a")
                  (text "Use" <+> colorizeKey "null a" <+> text "instead"))
    (sI, (List _ _ []), "/=",  _)
      -> report (Message
                  (getSpan sI)
                  (text "Do not use" <+> colorizeKey "[] /= a")
                  (text "Use" <+> colorizeKey "not (null a)" <+> text "instead"))
    (sI, _, "/=", (List _ _ []))
      -> report (Message
                  (getSpan sI)
                  (text "Do not use" <+> colorizeKey "a /= []")
                  (text "Use" <+> colorizeKey "not (null a)" <+> text "instead"))
    _ -> return ()

-- Checks whether a comparison operator is applied to the empty list 
-- (e.g., `(==) x []`). If so, a warning is emitted (use 'null' instead).
checkApply :: Expression a -> CSM ()
checkApply e =
  case checkApplyCompare e of
    (sI, "==", _ , (List _ _ []))
      -> report (Message
                  (getSpan sI)
                  (text "Do not use" <+> colorizeKey "(==) a []")
                  (text "Use" <+> colorizeKey "null a" <+> text "instead"))
    (sI, "==", (List _ _ []), _)
      -> report (Message
                  (getSpan sI)
                  (text "Do not use" <+> colorizeKey "(==) [] a")
                  (text "Use" <+> colorizeKey "null a" <+> text "instead"))
    (sI, "/=", (List _ _ []), _)
      -> report (Message
                  (getSpan sI)
                  (text "Do not use" <+> colorizeKey "(/=) [] a")
                  (text "Use" <+> colorizeKey "not (null a)" <+> text "instead"))
    (sI, "/=", _, (List _ _ []))
      -> report (Message
                  (getSpan sI)
                  (text "Do not use" <+> colorizeKey "(/=) a []")
                  (text "Use" <+> colorizeKey "not (null a)" <+> text "instead"))
    _ -> return ()
