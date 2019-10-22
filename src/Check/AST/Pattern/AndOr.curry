module Check.AST.Pattern.AndOr where

import Curry.SpanInfo
import Curry.Span
import Curry.Position
import Curry.Types
import Curry.Ident
import Text.Pretty

import Types

-- if found an apply on an apply, check
checkAndOr :: Expression a -> Int -> CSM ()
checkAndOr e _ =
  case e of
    (Apply
      sI
      (Apply _
        (Variable _ _
          (QualIdent _ _ (Ident _ _ _)))
        (Variable _ _
          (QualIdent _ _
            (Ident _ _ _))))
      _) -> checkAndOr' e
    _    -> return ()

-- if foldl/foldr is used with || or && use predefined functions 'and' and 'or'
checkAndOr' :: Expression a -> CSM ()
checkAndOr' e =
  case getAndOrAsTupel e of
    (sI, "foldr", "||")  -> (report (Message
                                      (getSpan sI)
                                      ( text "superfluous code"
                                      <+> colorizeKey "foldr ||"
                                      )
                                      ( text "instead of"
                                      <+> colorizeKey "foldr || False"
                                      <+> text "write"
                                      <+> colorizeKey "or")))
    (sI, "foldr", "&&")  -> (report (Message
                                      (getSpan sI)
                                      ( text "superfluous code"
                                      <+> colorizeKey "foldr && True"
                                      )
                                      ( text "instead of"
                                      <+> colorizeKey "foldr &&"
                                      <+> text "write"
                                      <+> colorizeKey "and")))
    (sI, "foldl", "||")  -> (report (Message
                                      (getSpan sI)
                                      ( text "superfluous code"
                                      <+> colorizeKey "foldl ||"
                                      )
                                      ( text "instead of"
                                      <+> colorizeKey "foldl || False"
                                      <+> text "write"
                                      <+> colorizeKey "or"
                                      <+> text "(better use of laziness)")))
    (sI, "foldl", "&&")  -> (report (Message
                                      (getSpan sI)
                                      ( text "superfluous code"
                                      <+> colorizeKey "foldr &&"
                                      )
                                      ( text "instead of"
                                      <+> colorizeKey "foldr && True"
                                      <+> text "write"
                                      <+> colorizeKey "and"
                                      <+> text "(better use of laziness)")))
    _                    -> return ()

-- return relevant parts of ast for catching foldr/foldr ||/&&
getAndOrAsTupel :: Expression a -> (SpanInfo, String, String)
getAndOrAsTupel
  (Apply
    sI
    (Apply _
      (Variable _ _
        (QualIdent _ _ (Ident _ func _)))
      (Variable _ _
        (QualIdent _ _
          (Ident _ inf _))))
    _)
  = (sI, func, inf)
