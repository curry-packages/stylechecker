module Check.AST.Pattern.NotEqual where

import Curry.SpanInfo
import Curry.Span
import Curry.Position
import Curry.Types
import Curry.Ident
import Text.Pretty

import Types

-- if not (a = b) or not (a /= b) used, recommend /= or ==
-- -- NOTE: add not ((==) a b)
checkNotEqual :: Expression a -> Int -> CSM ()
checkNotEqual e _ =
  case e of
    (Apply sI
      (Variable _ _
        (QualIdent _ _
          (Ident _ "not" _)))
      (Paren _
        (InfixApply
          _
          _
          (InfixOp _
            (QualIdent _ _
              (Ident _ "==" _)))
          _ )))
      -> report (Message
                  (getSpan sI)
                  (text "Do not use" <+> colorizeKey "not (a == b)")
                  (text "Use" <+> colorizeKey "a /= b" <+> text "instead"))
    (Apply sI
      (Variable _ _
        (QualIdent _ _
          (Ident _ "not" _)))
      (Paren _
        (InfixApply
          _
          _
          (InfixOp _
            (QualIdent _ _
              (Ident _ "/=" _)))
          _ )))
      -> report (Message
                  (getSpan sI)
                  (text "Do not use" <+> colorizeKey "not (a /= b)")
                  (text "Use" <+> colorizeKey "a == b" <+> text "instead"))
    (Apply sI
      (Variable _ _
        (QualIdent _ _
          (Ident _ "not" _)))
      (Paren _
        (Apply _
          (Apply _
            (Variable _ _
              (QualIdent _ _
                (Ident _ "/=" _)
              )
            )
          exp1)
        exp2)))
      -> report (Message
                  (getSpan sI)
                  (text "Do not use" <+> colorizeKey "not ((/=) a b)")
                  (text "Use" <+> colorizeKey "(==) a b" <+> text "instead"))
    (Apply sI
      (Variable _ _
        (QualIdent _ _
          (Ident _ "not" _)))
      (Paren _
        (Apply _
          (Apply _
            (Variable _ _
              (QualIdent _ _
                (Ident _ "==" _)
              )
            )
          exp1)
        exp2)))
      -> report (Message
                  (getSpan sI)
                  (text "Do not use" <+> colorizeKey "not ((==) a b)")
                  (text "Use" <+> colorizeKey "(/=) a b" <+> text "instead"))
    _ -> return ()
