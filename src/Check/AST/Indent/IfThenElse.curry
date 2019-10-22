module Check.AST.Indent.IfThenElse where

import Curry.SpanInfo
import Curry.Span
import Curry.Position
import Curry.Types
import Text.Pretty

import Types

-- per patternmatching apply actual check only on ifthenelse constructs
checkIfThenElse :: Expression a -> Int -> CSM ()
checkIfThenElse e _ =
  case e of
    (IfThenElse sI expr1 expr2 expr3) -> checkIfThenElse' sI
                                                          (getSpanInfo expr1)
                                                          (getSpanInfo expr2)
                                                          (getSpanInfo expr3)
    _                                 -> return ()

-- check if various situations:
-- all in one line
-- if - else in one line, else expression not
-- if - then in one line, else and else expression in another -> twolines
-- if - then in one line, then expression and else in one
-- if and then not in one line, if expression and then in one
-- if then else in different lines, if and then expressions not right in front
-- if then and else
checkIfThenElse' :: SpanInfo -> SpanInfo -> SpanInfo -> SpanInfo -> CSM ()
checkIfThenElse'
  (SpanInfo
    sp
    [ Span (Position lpi pi) _
    , Span (Position lpt pt) _
    , Span (Position lpe pe) _
    ]
  ) sI1@(SpanInfo (Span _ (Position li ci)) _)
    sI2@(SpanInfo (Span _ (Position lt ct)) _)
    sI3@(SpanInfo (Span _ (Position le ce)) _)
  |lpi == le               = return ()
  |lpi == lpe              = do report (Message
                                         sp
                                         ( text "wrong formatting"
                                           <+> colorizeKey "else expression"
                                         )
                                         ( text "do not break"
                                           <+> colorizeKey " else expression"
                                           <+> text "if writing"
                                           <+> colorizeKey "if-then-else"
                                           <+> text "in one line"
                                         ))
  |lpi == lpt && lt /= lpe = do checkIfThenElseInTwoLines sp pt pe
                                checkBreakIndent "then" lpt pt sI2
                                checkBreakIndent "else" lpe pe sI3
  |lt == lpe               = do report (Message
                                         sp
                                         ( text "wrong formatting"
                                           <+> colorizeKey "else"
                                         )
                                         ( colorizeKey "else"
                                           <+> text " should start in seperate line"
                                         ))
  |li == lpt               = do report (Message
                                         sp
                                         ( text "wrong formatting"
                                           <+> colorizeKey "then"
                                         )
                                         ( colorizeKey "then"
                                           <+> text " should start in seperate line"
                                         ))
  |li /= lpt && lt /= lpe  = do checkIfThenElseInThreeLines sp pi pt pe
                                checkBreakIndent "if" lpi pi sI1
                                checkBreakIndent "then" lpt pt sI2
                                checkBreakIndent "else" lpe pe sI3
  -- |otherwise

-- then and else should alway be aligned
checkIfThenElseInTwoLines :: Span -> Int -> Int -> CSM ()
checkIfThenElseInTwoLines sp pt pe =
  unlessM (pt == pe) (report (Message
                                sp
                                ( colorizeKey "then"
                                  <+> text "and"
                                  <+> colorizeKey "else"
                                  <+> text "wrong alignement"
                                  )
                                ( text "align"
                                  <+> colorizeKey "then"
                                  <+> text "and"
                                  <+> colorizeKey "else"
                                  )
                              )
                      )

-- see above, but also, if then starts in another line, indent by 2
checkIfThenElseInThreeLines :: Span -> Int -> Int -> Int -> CSM ()
checkIfThenElseInThreeLines sp pi pt pe =
  if pt == pe
    then unlessM (pe-pi == 2) (report (Message
                                        sp
                                        ( colorizeKey "then"
                                          <+> text "and"
                                          <+> colorizeKey "else"
                                          <+> text "not properly indented"
                                        )
                                        ( text "indent by 2 from"
                                          <+> colorizeKey "if"
                                        )
                                      )
                              )
    else report (Message
                    sp
                    ( colorizeKey "then"
                      <+> text "and"
                      <+> colorizeKey "else"
                      <+> text "not aligned"
                      )
                      ( text "align"
                        <+> colorizeKey "then"
                        <+> text "and"
                        <+> colorizeKey "else"
                        )
                  )

-- gets keyname as string, keypositions and the expression to check
-- if the expression is in next line, check indent
checkBreakIndent :: String -> Int -> Int -> SpanInfo -> CSM ()
checkBreakIndent s l c sI = do
  unlessM (l == (getLi sI))
          (unlessM ((getCol sI)==(c+2))
                   (report (Message
                             (getSpan sI)
                             ( colorizeKey "expression"
                               <+> text "wrongly indented"
                             )
                             ( colorizeKey(s ++ " expression")
                               <+> text "should be indented by 2 from"
                               <+> colorizeKey s
                             )
                           )
                  )
          )
