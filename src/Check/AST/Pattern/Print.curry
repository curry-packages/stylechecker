module Check.AST.Pattern.Print where

import Curry.SpanInfo
import Curry.Span
import Curry.Position
import Curry.Types
import Curry.Ident
import Text.Pretty

import Types

-- Finds pattern `putStrLn (show a)` and recommends `print` instead.
checkPrint :: Expression a -> Int -> CSM ()
checkPrint e _ =
  case e of
    (Apply
      sI
      (Variable
        _
        _
        (QualIdent
          _
          _
          (Ident _ "putStrLn" _)))
      (Paren
        _
        (Apply
          _
          (Variable
            _
            _
            (QualIdent
              _
              _
              (Ident _ "show" _)))
          _))) -> (report (Message
                            (getSpan sI)
                            ( text "superfluous code"
                            <+> colorizeKey "putStrLn (show a) "
                            )
                            ( text "instead of"
                            <+> colorizeKey "putStrLn (show a)"
                            <+> text "write"
                            <+> colorizeKey "print a")))
    _      -> return ()

