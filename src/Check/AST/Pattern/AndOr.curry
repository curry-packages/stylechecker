module Check.AST.Pattern.AndOr where

import Curry.SpanInfo
import Curry.Span
import Curry.Position
import Curry.Types
import Curry.Ident
import Text.Pretty

import Types

-- Applies the 'AndOr' check on the given expression, if applicable.
checkAndOr :: Expression a -> Int -> CSM ()
checkAndOr e _ =
  case (getAndOrAsTupel e) of
    Just n -> checkAndOr' n
    _      -> return ()

-- If `foldl` / `foldr` is used with `||` or `&&`, a warning is emitted (use 'and' and 'or' instead).
checkAndOr' :: (SpanInfo, String, String) -> CSM ()
checkAndOr' n =
  case n of
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

-- Returns relevant parts of ast for catching combinations of `foldr` / `foldr` and `||` / `&&`.
getAndOrAsTupel :: Expression a -> Maybe (SpanInfo, String, String)
getAndOrAsTupel e = case e of 
  (Apply
    sI
    (Apply _
      (Variable _ _
        (QualIdent _ _ (Ident _ func _)))
      (Variable _ _
        (QualIdent _ _
          (Ident _ inf _))))
    _) -> Just (sI, func, inf)
  _ -> Nothing