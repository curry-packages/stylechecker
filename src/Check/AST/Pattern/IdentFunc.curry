module Check.AST.Pattern.IdentFunc where

import Curry.SpanInfo
import Curry.Span
import Curry.Position
import Curry.Types
import Curry.Ident
import Text.Pretty

import Types

-- instead of lambda function \x -> x use id
checkIdentFunc :: Expression a -> Int -> CSM ()
checkIdentFunc e _ =
  case e of
    (Lambda
      sI
      [(VariablePattern _ _ ident1)]
      (Variable _ _ (QualIdent _ _ ident2))) -> whenM (ident1 == ident2)
                                                  (report ( Message
                                                            (getSpan sI)
                                                            ( text "superfluous code"
                                                            <+> colorizeKey "\\x -> x"
                                                            )
                                                            ( text "instead of"
                                                            <+> colorizeKey "\\x -> x"
                                                            <+> text "write"
                                                            <+> colorizeKey "id"
                                                            )
                                                          )
                                                  )
    _                                       -> return ()
