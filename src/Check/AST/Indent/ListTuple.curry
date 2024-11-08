module Check.AST.Indent.ListTuple where

import Control.Monad  ( unless )
import Curry.SpanInfo
import Curry.Span
import Curry.Position
import Curry.Types
import Curry.Ident
import Text.Pretty

import Types

-- Applies actual check on List constructs.
checkListTuple :: Rhs a -> Int -> CSM ()
checkListTuple e i =
  case e of
    (SimpleRhs sI _ (List sC _ exps) _) -> checkListTuple' sI sC exps i "list"
    (SimpleRhs sI _ (Tuple  sC exps) _) -> checkListTuple' sI sC exps i "tuple"
    _ -> return ()

-- Checks indentation and alignment if a declaration is not in one line.
checkListTuple' :: SpanInfo -> SpanInfo -> [Expression a] -> Int -> String -> CSM ()
checkListTuple' si1 si2 (exp:exps) i s = case (si1, si2) of 
  ((SpanInfo rhsSp@(Span (Position eql1 _) (Position _ _))  _), 
   (SpanInfo       (Span (Position l1 c1)  (Position l2 _)) symbSpans)) ->  do
    unless (eql1 == l1)
      (unless ((i+2) == c1)
        (report (Message rhsSp
                        (colorizeKey (s ++ " declaration") <+> text "wrong formatting")
                        ( text "either write"
                        <+> colorizeKey "right-hand-side"
                        <+> text "right after the equation sign or start in new line and 2 indentation"
                        )
                )
        )
      )
    unless (l1 == l2)
      (if (spanAlign symbSpans)
        then
          (unless (checkAlign getCol (getCol (getSpanInfo exp)) exps)
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
  _ -> return ()
checkListTuple' _ _ [] _ _ = return ()
