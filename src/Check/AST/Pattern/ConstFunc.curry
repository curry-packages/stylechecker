module Check.AST.Pattern.ConstFunc where

import Curry.SpanInfo
import Curry.Span
import Curry.Position
import Curry.Types
import Curry.Ident
import Text.Pretty

import Types

-- in case of lambda function \x y -> x
checkConstFunc :: Expression a -> Int -> CSM ()
checkConstFunc e _ =
  case e of
    (Lambda
      sI
      [ (VariablePattern _ _ ident1)
      , (VariablePattern _ _ ident2)]
      (Variable _ _ (QualIdent _ _ ident3))) -> do whenM (ident1 == ident3)
                                                     (report ( Message
                                                               (getSpan sI)
                                                               ( text "superfluous code"
                                                               <+> colorizeKey "\\x y -> x"
                                                               )
                                                               ( text "instead of"
                                                               <+> colorizeKey "\\x y -> x"
                                                               <+> text "write"
                                                               <+> colorizeKey "const"
                                                               )
                                                             )
                                                     )
                                                   whenM (ident3 == ident2)
                                                     (report ( Message
                                                               (getSpan sI)
                                                               ( text "superfluous code"
                                                               <+> colorizeKey "\\x y -> y"
                                                               )
                                                               ( text "instead of"
                                                               <+> colorizeKey "\\x y -> y"
                                                               <+> text "write"
                                                               <+> colorizeKey "const"
                                                               <+> text "and switch parameters"
                                                               )
                                                             )
                                                     )
    _                                       -> return ()
