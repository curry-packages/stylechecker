module Check.AST.Pattern.EqualsTrue where

import Curry.SpanInfo
import Curry.Span
import Curry.Position
import Curry.Types
import Curry.Ident
import Text.Pretty

import Types

-- calls actual checks on correct patterns:
-- 1. a == b
-- 2. (==) a b
checkEqualsTrue :: Expression a -> Int -> CSM ()
checkEqualsTrue e _ =
  case e of
    (InfixApply
      _
      _
      (InfixOp _
        (QualIdent _ _
          (Ident _ _ _)))
      _)
      -> checkInfixEqualsTrue e
    (Apply _
      (Apply _
        (Variable _ _
          (QualIdent _ _
            (Ident _ _ _)
          )
        )
        _)
      _)
      -> checkApplyEqualsTrue e
    _ -> return ()

-- for each true, false and ==, /= combination with a bool, recommend just using
-- the bool
checkInfixEqualsTrue :: Expression a -> CSM ()
checkInfixEqualsTrue e = case checkInfixCompare e of
  (sI, (Constructor _ _ (QualIdent _ _ (Ident _ "True" _))), "==", _)     -> warnEqualsTrue sI "True == a"     "a"
  (sI, (Constructor _ _ (QualIdent _ _ (Ident _ "False" _))) , "==", _)   -> warnEqualsTrue sI "False == a"    "not a"
  (sI, (Constructor _ _ (QualIdent _ _ (Ident _ "True" _))) , "/=", _)    -> warnEqualsTrue sI "True /= a"     "not a"
  (sI, (Constructor _ _ (QualIdent _ _ (Ident _ "False" _))) , "/=", _)   -> warnEqualsTrue sI "False /= a"    "a"
  (sI, _ , "==", (Constructor _ _ (QualIdent _ _ (Ident _ "True" _))))    -> warnEqualsTrue sI "a == True"     "a"
  (sI, _ , "==", (Constructor _ _ (QualIdent _ _ (Ident _ "False" _))))   -> warnEqualsTrue sI "a == False"    "not a"
  (sI, _ , "/=", (Constructor _ _ (QualIdent _ _ (Ident _ "True" _))))    -> warnEqualsTrue sI "a /= True"     "not a"
  (sI, _ , "/=", (Constructor _ _ (QualIdent _ _ (Ident _ "False" _))))   -> warnEqualsTrue sI "a /= False"    "a"
  _                                                                       -> return ()

checkApplyEqualsTrue :: Expression a -> CSM ()
checkApplyEqualsTrue e = case checkApplyCompare e of
  (sI, "==", (Constructor _ _ (QualIdent _ _ (Ident _ "True" _))), _)     -> warnEqualsTrue sI "(==) True a"   "a"
  (sI, "==", (Constructor _ _ (QualIdent _ _ (Ident _ "False" _))), _)    -> warnEqualsTrue sI "(==) False a"  "not a"
  (sI, "/=", (Constructor _ _ (QualIdent _ _ (Ident _ "True" _))), _)     -> warnEqualsTrue sI "(/=) True a"   "not a"
  (sI, "/=", (Constructor _ _ (QualIdent _ _ (Ident _ "False" _))), _)    -> warnEqualsTrue sI "(/=) False a"  "a"
  (sI, "==", _ , (Constructor _ _ (QualIdent _ _ (Ident _ "True" _))))    -> warnEqualsTrue sI "(==) a True"   "a"
  (sI, "==", _ , (Constructor _ _ (QualIdent _ _ (Ident _ "False" _))))   -> warnEqualsTrue sI "(==) a False"  "not a"
  (sI, "/=", _ , (Constructor _ _ (QualIdent _ _ (Ident _ "True" _))))    -> warnEqualsTrue sI "(/=) a True"   "not a"
  (sI, "/=", _ , (Constructor _ _ (QualIdent _ _ (Ident _ "False" _))))   -> warnEqualsTrue sI "(/=) a False"  "a"
  _                                                                       -> return ()

-- report
warnEqualsTrue :: SpanInfo -> String -> String -> CSM ()
warnEqualsTrue sI dn d =
  do
    report (Message
             (getSpan sI)
             (text "superflous code")
             (text "Instead of"
             <+> colorizeKey dn
             <+> text "use"
             <+> colorizeKey d))
