module Check.AST.Indent.Guard where

import Curry.SpanInfo
import Curry.Span
import Curry.Position
import Curry.Types
import Text.Pretty
import Control.Monad ( unless )

import Types

-- Aplies actual check on Guards, the function lefthand side is needed
-- for positions.
checkGuard :: Equation a -> Int -> CSM ()
checkGuard e i =
  case e of
    (Equation _ _ (FunLhs fSI _ _) (GuardedRhs gSI _ cExprs _))
                       -> checkGuard' fSI gSI cExprs i
    _                  -> return ()

-- If bars are aligned, this function checks their indent, otherwise it 
-- reports same for equal signs, if one starts in same line as its guard,
-- disregard indentation.
checkGuard' :: SpanInfo -> SpanInfo -> [CondExpr a] -> Int -> CSM ()
checkGuard' p sI (cExpr:cExprs) i = case p of 
  (SpanInfo (Span (Position lf _) _) _) -> do
    if checkAlign getCol (getCol (getSpanInfo cExpr)) cExprs
      then checkGuardIndent sI lf cExpr i
      else report (Message (getSpan sI)
                    (colorizeKey "guards" <+> text "not aligned")
                    (text "align" <+> colorizeKey "guards"))
    if checkAlign getEqualCol (getEqualCol (getSpanInfo cExpr)) cExprs
      then (unless (guardsEqualSSameLine (cExpr:cExprs)) (checkEqualIndent sI cExpr))
      else report (Message (getSpan sI)
                    (colorizeKey "guard" <+> text "equal signs not aligned")
                    (text "align" <+> colorizeKey "guard" <+> text "equal signs"))
  _ -> return ()
checkGuard' _ _ [] _ = return ()

-- Checks if they are indented by 2 from edge or start in same line as functionlhs.
checkGuardIndent :: SpanInfo -> Int -> CondExpr a -> Int -> CSM ()
checkGuardIndent sI f cExpr i =
  let posCExprEq = (getSpanInfo cExpr)
  in unless ((getCol posCExprEq) == (i+2) && ((getLi posCExprEq) == f))
        $ report (Message (getSpan sI)
                     (colorizeKey "guards" <+> text "wrong indentation")
                     (text "indent by 2 from" <+> colorizeKey "outer block"))

-- Checks if one equal sign continues in the same line as its guard.
guardsEqualSSameLine :: [CondExpr a] -> Bool
guardsEqualSSameLine (cExpr:cExprs) =
  (getLi (getSpanInfo cExpr) == (getEqualLi (getSpanInfo cExpr))) || guardsEqualSSameLine cExprs
guardsEqualSSameLine []             = True

-- Checks if they are aligned with guards.
checkEqualIndent :: SpanInfo -> CondExpr a -> CSM ()
checkEqualIndent sI cExpr =
  let posCExprEq = (getSpanInfo cExpr)
      l = getLi sI
  in unless ((getLi posCExprEq) == l)
             $ report (Message (getSpan sI) (colorizeKey "equal signs" <+> text "wrong indentation")
                                            (text "indent by 2 from" <+> colorizeKey "outer block"))

-- Returns column position of equal sign from spaninfo.
getEqualCol :: SpanInfo -> Int
getEqualCol si = case si of
  (SpanInfo _ [ _ , Span (Position _ c) _]) -> c
  _ -> error "getEqualCol: NoSpanInfo"

-- Returns line position of equal sign from spaninfo.
getEqualLi :: SpanInfo -> Int
getEqualLi si = case si of 
  (SpanInfo _ [ _ , Span (Position l _) _]) -> l
  _ -> error "getEqualLi: NoSpanInfo"
