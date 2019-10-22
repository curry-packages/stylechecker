module Check.AST.Pattern.NotOrd where

import Curry.SpanInfo
import Curry.Span
import Curry.Position
import Curry.Types
import Curry.Ident
import Text.Pretty

import Types

-- check if not (a op b) construct
checkNotOrd :: Expression a -> Int -> CSM ()
checkNotOrd e _ =
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
              (Ident _ op _)))
          _ )))
      -> checkNotOrd' sI op warnNotOrd
    (Apply sI
      (Variable _ _
        (QualIdent _ _
          (Ident _ "not" _)))
      (Paren _
        (Apply _
          (Apply _
            (Variable _ _
              (QualIdent _ _
                (Ident _ op _)
              )
            )
          _)
        _)))
      ->  checkNotOrd' sI op warnNotOrd'
    _ -> return ()

-- case compareoperations are used, warn with corrections
checkNotOrd' :: SpanInfo -> String -> (String -> String -> SpanInfo -> CSM()) -> CSM ()
checkNotOrd' sI s f = case s of
  "<"  -> f s ">=" sI
  "<=" -> f s ">" sI
  ">"  -> f s "<=" sI
  ">=" -> f s "<" sI
  _    -> return ()

-- report for infix
warnNotOrd :: String -> String -> SpanInfo -> CSM ()
warnNotOrd s1 s2 sI = report (Message
                               (getSpan sI)
                               (text "Do not use" <+> colorizeKey ("not (a "++s1++" b)"))
                               (text "Use" <+> colorizeKey ("a "++s2++" b") <+> text "instead"))

-- report for apply
warnNotOrd' :: String -> String -> SpanInfo -> CSM ()
warnNotOrd' s1 s2 sI = report (Message
                                (getSpan sI)
                                (text "Do not use" <+> colorizeKey ("not (("++s1++") a b)"))
                                (text "Use" <+> colorizeKey ("("++s2++") a b") <+> text "instead"))

