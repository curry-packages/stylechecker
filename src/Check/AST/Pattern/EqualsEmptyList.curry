module Check.AST.Pattern.EqualsEmptyList where

import Curry.SpanInfo
import Curry.Span
import Curry.Position
import Curry.Types
import Curry.Ident
import Text.Pretty

import Types

-- check for infix operation and function with two parameters
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

-- if compared with [] use function null instead
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

-- if compared with [] use function null instead
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
