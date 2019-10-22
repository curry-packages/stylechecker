module Check.AST.Indent.ListTuple where

import Curry.SpanInfo
import Curry.Span
import Curry.Position
import Curry.Types
import Curry.Ident
import Text.Pretty

import Types

-- applies actual check on List constructs
checkListTuple :: Rhs a -> Int -> CSM ()
checkListTuple e i =
  case e of
    (SimpleRhs sI (List sC _ exps@(l:ls)) _) -> checkListTuple' sI sC exps i "list"
    (SimpleRhs sI (Tuple sC exps) _)  -> checkListTuple' sI sC exps i "tuple"
    _                    -> return ()

-- check indentation and alignment if declaration not in one line
checkListTuple' :: SpanInfo -> SpanInfo -> [Expression a] -> Int -> String -> CSM ()
checkListTuple' (SpanInfo rhsSp@(Span (Position eql1 eqc1) (Position eql2 eqc2)) _)
                (SpanInfo (Span (Position l1 c1) (Position l2 c2)) symbSpans)
                (exp:exps)
                i
                s
                = do
  unlessM (eql1 == l1)
    (unlessM ((i+2) == c1)
      (report (Message rhsSp
                       (colorizeKey (s ++ " declaration") <+> text "wrong formatting")
                       ( text "either write"
                       <+> colorizeKey "right-hand-side"
                       <+> text "right after the equation sign or start in new line and 2 indentation"
                       )
              )
      )
    )
  unlessM (l1 == l2)
    (if (spanAlign symbSpans)
       then
         (unlessM (checkAlign getCol (getCol (getSpanInfo exp)) exps)
           (report (Message rhsSp
                       (colorizeKey (s ++ " elements") <+> text "not aligned")
                       ( text "align"
                       <+> colorizeKey "list elements"
                       )
                   )
           )
           )
       else
         (report (Message rhsSp
                       (colorizeKey (s ++ " symbols") <+> text "not aligned")
                       ( text "align"
                       <+> colorizeKey "("
                       <+> text "or"
                       <+> colorizeKey "["
                       <> text ","
                       <+> colorizeKey ")"
                       <+> text "or"
                       <+> colorizeKey "]"
                       <+> text "and"
                       <+> colorizeKey ","
                       )
                 )
         )
    )
