module Check.AST.Indent.IfThenElse where

import Curry.SpanInfo
import Curry.Span
import Curry.Position
import Curry.Types
import Text.Pretty
import Control.Monad ( unless )

import Types

-- Applies actual check only on if-then-else constructs by
-- pattern-matching on the expression.
checkIfThenElse :: Expression a -> Int -> CSM ()
checkIfThenElse e _ =
  case e of
    (IfThenElse sI expr1 expr2 expr3) -> checkIfThenElse' sI
                                                          (getSpanInfo expr1)
                                                          (getSpanInfo expr2)
                                                          (getSpanInfo expr3)
    _                                 -> return ()

-- Checks for various situations:
--   all in one line
--   if - else in one line, else expression not
--   if - then in one line, else and else expression in another -> twolines
--   if - then in one line, then expression and else in one
--   if and then not in one line, if expression and then in one
--   if then else in different lines, if and then expressions not right in front
--   if then and else
checkIfThenElse' :: SpanInfo -> SpanInfo -> SpanInfo -> SpanInfo -> CSM ()
checkIfThenElse' sI0 sI1 sI2 sI3 = case (sI0, sI1, sI2, sI3) of
  (SpanInfo sp [ Span (Position lpi pi) _, Span (Position lpt pt) _, Span (Position lpe pe) _],
   SpanInfo (Span _ (Position li _)) _,
   SpanInfo (Span _ (Position lt _)) _,
   SpanInfo (Span _ (Position le _)) _)
    | lpi == le               -> return ()
    | lpi == lpe              -> do report (Message
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
    | lpi == lpt && lt /= lpe -> do checkIfThenElseInTwoLines sp pt pe
                                    checkBreakIndent "then" lpt pt sI2
                                    checkBreakIndent "else" lpe pe sI3
    | lt == lpe               -> do report (Message
                                            sp
                                            ( text "wrong formatting"
                                             <+> colorizeKey "else"
                                            )
                                            ( colorizeKey "else"
                                              <+> text " should start in seperate line"
                                            ))
    | li == lpt               -> do report (Message
                                            sp
                                            ( text "wrong formatting"
                                              <+> colorizeKey "then"
                                            )
                                            ( colorizeKey "then"
                                              <+> text " should start in seperate line"
                                            ))
    | li /= lpt && lt /= lpe  -> do checkIfThenElseInThreeLines sp pi pt pe
                                    checkBreakIndent "if" lpi pi sI1
                                    checkBreakIndent "then" lpt pt sI2
                                    checkBreakIndent "else" lpe pe sI3
  _ -> return ()

-- `then` and `else` should alway be aligned.
checkIfThenElseInTwoLines :: Span -> Int -> Int -> CSM ()
checkIfThenElseInTwoLines sp pt pe =
  unless (pt == pe) (report (Message
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

-- See above, but also, if then starts in another line, indent by 2.
checkIfThenElseInThreeLines :: Span -> Int -> Int -> Int -> CSM ()
checkIfThenElseInThreeLines sp pi pt pe =
  if pt == pe
    then unless (pe-pi == 2) (report (Message
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

-- Gets keyname as string, keypositions and the expression to check
-- if the expression is in next line, checks indent.
checkBreakIndent :: String -> Int -> Int -> SpanInfo -> CSM ()
checkBreakIndent s l c sI = do
  unless (l == (getLi sI))
          (unless ((getCol sI)==(c+2))
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
