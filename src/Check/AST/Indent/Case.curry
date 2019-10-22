module Check.AST.Indent.Case where

import Curry.SpanInfo
import Curry.Span
import Curry.Position
import Curry.Types
import Text.Pretty

import Types

-- applies actual check on Case constructs
checkCase :: Expression a -> Int -> CSM ()
checkCase e i =
  case e of
    (Case sI t expr alts) -> checkCase' sI alts i
    _                     -> return ()

-- check alignment of arrows and case alternatives.
-- if alternatives are aligned,
-- check indentation
checkCase' :: SpanInfo -> [Alt a] -> Int -> CSM ()
checkCase' sI (alt:alts) i = do
  unlessM (checkAlign getAltArrowCol (getAltArrowCol (getSpanInfo alt)) alts)
    $ report (Message (getSpan sI)
                ((colorizeKey "case") <+> text "arrows not aligned") (text "align the arrows"))
  if checkAlign getCol (getCol (getSpanInfo alt)) alts
    then altIndent sI alt i
    else report (Message (getSpan sI)
                  ((colorizeKey "case") <+> text "options not aligned") (text "align the options"))

-- alternatives should be in the next line from case, and indented by 2 from
-- outer edge or case
altIndent :: SpanInfo -> Alt a -> Int -> CSM ()
altIndent sI alt i =
  let cAlt = (getCol (getSpanInfo alt))
  in
    unlessM ((cAlt == ((getCol sI)+2)) || (cAlt == (i+2)))
      $ report (Message (getSpan sI)
                  ( (colorizeKey "case") <+> text "options wrong indention")
                  ( text "start alternatives in next line and indent by 2 spaces from"
                    <+> colorizeKey "case"
                    <+> text "or" <+> colorizeKey "outer structur"
                    <+> text "; if"
                    <+> colorizeKey "case"
                    <+> text "is the topmost control structur from start of line"))

--gets column positions of an arrow in a alternativ in case
getAltArrowCol :: SpanInfo -> Int
getAltArrowCol (SpanInfo _ [Span (Position _ c) _]) = c

