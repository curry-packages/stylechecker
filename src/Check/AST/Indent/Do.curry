module Check.AST.Indent.Do where

import Curry.SpanInfo
import Curry.Span
import Curry.Position
import Curry.Types
import Text.Pretty

import Types

-- applies actual check on Do constructs
checkDo :: Expression a -> Int -> CSM ()
checkDo e i =
  case e of
    (Do sI sts expr) -> checkDo' sI sts expr i
    _                -> return ()

-- check if to stametments and expression are aligned, else warn,
-- if aligned, check indentation, if first statement in same line as do
-- all is well, else, see if they are indented by two from do or outer Block
checkDo' :: SpanInfo -> [Statement a] -> Expression a -> Int -> CSM ()
checkDo' (SpanInfo (Span (Position lp p) eP) _) (st:sts) expr i =
  if checkDoStatements (getCol (getSpanInfo st)) sts expr
    then
      unlessM ((getLi (getSpanInfo st)) == lp)
        (do let colSts = (getCol (getSpanInfo st))
            unlessM ((colSts == (i+2)) ||(colSts == p+2))
              (report (Message (Span (Position lp p) eP)
                          (colorizeKey "do" <+> text "body wrong indention")
                          ( text "indent by 2"
                            <+> colorizeKey "spaces"
                            <+> text "from"
                            <+> colorizeKey "do"
                            <+> text " or"
                            <+> colorizeKey "outer block"
                            )
                      )
              )
      )
    else report (Message (Span (Position lp p) eP)
                  ((colorizeKey "do") <+> text "body not aligned") (text "align body"))
-- if no statements exist, check only indentation
checkDo' (SpanInfo (Span (Position lp p) eP) _) [] expr i       =
  unlessM ((getLi (getSpanInfo expr)) == lp)
    (do let colSts = (getCol (getSpanInfo expr))
        unlessM ((colSts == (i+2) ||(colSts == p+2)))
          (report (Message (Span (Position lp p) eP)
                      (colorizeKey "do" <+> text "body wrong indention")
                      ( text "indent by 2"
                        <+> colorizeKey "spaces"
                        <+> text "from"
                        <+> colorizeKey "do"
                        <+> text " or"
                        <+> colorizeKey "outer block"
                      )
                  )
          )
   )

--check if statements and Expression are aligned
checkDoStatements :: Int -> [Statement a] -> Expression a -> Bool
checkDoStatements p (st:sts) expr =
  (p == getCol (getSpanInfo st)) && checkDoStatements (getCol (getSpanInfo st)) sts expr
checkDoStatements p []       expr = (p == getCol (getSpanInfo expr))
