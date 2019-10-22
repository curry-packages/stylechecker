module Check.AST.Pattern.ThenTrueElseFalse where

import Curry.SpanInfo
import Curry.Span
import Curry.Position
import Curry.Types
import Curry.Ident
import Text.Pretty

import Types

-- per patternmatching apply actual check only on ifthenelse constructs
checkThenTrueElseFalse :: Expression a -> Int -> CSM ()
checkThenTrueElseFalse e _ =
  case e of
    (IfThenElse sI _
      (Constructor _ _ (QualIdent _ _ (Ident _ "True" _)))
      (Constructor _ _ (QualIdent _ _ (Ident _ "False" _)))) -> checkThenTrueElseFalse' sI "not "
    (IfThenElse sI _
      (Constructor _ _ (QualIdent _ _ (Ident _ "False" _)))
      (Constructor _ _ (QualIdent _ _ (Ident _ "True" _))))  -> checkThenTrueElseFalse' sI ""
    _                                                        -> return ()

-- report
checkThenTrueElseFalse' :: SpanInfo -> String -> CSM ()
checkThenTrueElseFalse' (SpanInfo sp sps) s =
  report ( Message
           sp
           ( text "superfluous code"
           <+> colorizeKey "then bool else not (bool)"
           )
           ( text "write instead of if"
           <+> colorizeKey "condition"
           <+> text "then True (False) else False (True) just "
           <+> colorizeKey (s ++ "condition")
           )
         )
