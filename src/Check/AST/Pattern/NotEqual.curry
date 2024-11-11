module Check.AST.Pattern.NotEqual where

import Curry.SpanInfo
import Curry.Span
import Curry.Position
import Curry.Types
import Curry.Ident
import Text.Pretty

import Types

-- If `not (a = b)` or `not (a /= b)` is used, `/=` or `==` is recommended instead.
-- -- NOTE: add `not ((==) a b)`
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
          _)
        _)))
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
          _)
        _)))
      -> report (Message
                  (getSpan sI)
                  (text "Do not use" <+> colorizeKey "not ((==) a b)")
                  (text "Use" <+> colorizeKey "(/=) a b" <+> text "instead"))
    _ -> return ()
