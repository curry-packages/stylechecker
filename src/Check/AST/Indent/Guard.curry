module Check.AST.Indent.Guard where

import Curry.SpanInfo
import Curry.Span
import Curry.Position
import Curry.Types
import Text.Pretty

import Types

-- aplies actual check on Guards, the function lefthand side is needed
-- for positions
checkGuard :: Equation a -> Int -> CSM ()
checkGuard e i =
  case e of
    (Equation _ (FunLhs fSI _ _) (GuardedRhs gSI cExprs _))
                     -> checkGuard' fSI gSI cExprs i
    _                -> return ()

-- if bars are aligned check their indent, else report
-- same for equal signs, if one starts in same line as its guard,
-- disregard indentation
checkGuard' ::SpanInfo -> SpanInfo -> [CondExpr a] -> Int -> CSM ()
checkGuard' (SpanInfo (Span (Position lf _) _) _)
            sI
            (cExpr:cExprs) i = do
  if checkAlign getCol (getCol (getSpanInfo cExpr)) cExprs
    then checkGuardIndent sI lf cExpr i
    else report (Message (getSpan sI)
                  (colorizeKey "guards" <+> text "not aligned")
                  (text "align" <+> colorizeKey "guards"))
  if checkAlign getEqualCol (getEqualCol (getSpanInfo cExpr)) cExprs
    then (unlessM (guardsEqualSSameLine (cExpr:cExprs)) (checkEqualIndent sI cExpr))
    else report (Message (getSpan sI)
                  (colorizeKey "guard" <+> text "equal signs not aligned")
                  (text "align" <+> colorizeKey "guard" <+> text "equal signs"))

-- check if they are indented by 2 from edge or start in same line as functionlhs
checkGuardIndent :: SpanInfo -> Int -> CondExpr a -> Int -> CSM ()
checkGuardIndent sI f cExpr i =
  let posCExprEq = (getSpanInfo cExpr)
      l = getLi sI
      c = getCol sI
  in
    unlessM ((getCol posCExprEq) == (i+2))
      (unlessM ((getLi posCExprEq) == f)
        $ report (Message (getSpan sI)
                     (colorizeKey "guards" <+> text "wrong indentation")
                     (text "indent by 2 from" <+> colorizeKey "outer block")))

-- check if one equal sign continues in the same line as its guard
guardsEqualSSameLine :: [CondExpr a] -> Bool
guardsEqualSSameLine (cExpr:cExprs) =
  (getLi (getSpanInfo cExpr) == (getEqualLi (getSpanInfo cExpr))) || guardsEqualSSameLine cExprs
guardsEqualSSameLine []             = True

-- check if they are aligned with guards
checkEqualIndent :: SpanInfo -> CondExpr a -> CSM ()
checkEqualIndent sI cExpr =
  let posCExprEq = (getSpanInfo cExpr)
      l = getLi sI
  in unlessM ((getLi posCExprEq) == l)
             $ report (Message (getSpan sI) (colorizeKey "equal signs" <+> text "wrong indentation")
                                            (text "indent by 2 from" <+> colorizeKey "outer block"))

-- get column position of equal sign from spaninfo
getEqualCol :: SpanInfo -> Int
getEqualCol (SpanInfo _ [ _ , Span (Position _ c) _]) = c

-- get line position of equal sign from spaninfo
getEqualLi :: SpanInfo -> Int
getEqualLi (SpanInfo _ [ _ , Span (Position l _) _]) = l
