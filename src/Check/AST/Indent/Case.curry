module Check.AST.Indent.Case where

import Curry.SpanInfo
import Curry.Span
import Curry.Position
import Curry.Types
import Text.Pretty

import Control.Monad ( unless )

import Types

-- Applies actual check on `Case` constructs.
checkCase :: Expression a -> Int -> CSM ()
checkCase e i =
  case e of
    (Case sI _ _ _ alts) -> checkCase' sI alts i
    _                    -> return ()

-- Checks alignment of arrows and case alternatives.
-- If alternatives are aligned, indentation is checked.
checkCase' :: SpanInfo -> [Alt a] -> Int -> CSM ()
checkCase' _  []         _ = return ()
checkCase' sI (alt:alts) i = do
  unless (checkAlign getAltArrowCol (getAltArrowCol (getSpanInfo alt)) alts)
    $ report (Message (getSpan sI)
                ((colorizeKey "case") <+> text "arrows not aligned") (text "align the arrows"))
  if checkAlign getCol (getCol (getSpanInfo alt)) alts
    then altIndent sI alt i
    else report (Message (getSpan sI)
                  ((colorizeKey "case") <+> text "options not aligned") (text "align the options"))

-- Alternatives should be in the next line from case, and indented by 2 from
-- outer edge or case.
altIndent :: SpanInfo -> Alt a -> Int -> CSM ()
altIndent sI alt i =
  let cAlt = (getCol (getSpanInfo alt))
  in
    unless ((cAlt == ((getCol sI)+2)) || (cAlt == (i+2)))
      $ report (Message (getSpan sI)
                  ( (colorizeKey "case") <+> text "options wrong indention")
                  ( text "start alternatives in next line and indent by 2 spaces from"
                    <+> colorizeKey "case"
                    <+> text "or" <+> colorizeKey "outer structur"
                    <+> text "; if"
                    <+> colorizeKey "case"
                    <+> text "is the topmost control structur from start of line"))

-- Gets column positions of an arrow in a alternativ in `Case`.
getAltArrowCol :: SpanInfo -> Int
getAltArrowCol sp = case sp of
  (SpanInfo _ [Span (Position _ c) _]) -> c
  _ -> error "getAltArrowCol: NoSpan"