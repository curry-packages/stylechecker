module Check.AST.Indent.Deriving where

import Data.List     ( last )
import Control.Monad ( unless )

import Curry.SpanInfo
import Curry.Span
import Curry.Position
import Curry.Types
import Curry.Ident
import Text.Pretty

import Types

-- Applies actual check on data declarations.
checkDeriving :: Decl a -> Int -> CSM ()
checkDeriving e _ =
  case e of
    (DataDecl sI _ _ constr derivs) -> checkDeriving' sI constr derivs
    _                               -> return ()

-- Checks according to type.
checkDeriving' :: SpanInfo -> [ConstrDecl] -> [QualIdent] -> CSM ()
checkDeriving' sI (l:ls) d = case l of
  (ConstrDecl _ _ _)  -> checkDerivingC sI (l:ls) d
  (RecordDecl _ _ _)  -> checkRecord    sI (l:ls) d
  _                   -> return ()
checkDeriving' _ [] _ = return ()

-- Checks formatting.
checkDerivingC :: SpanInfo -> [ConstrDecl] -> [QualIdent]-> CSM ()
checkDerivingC si cons derivs@(deriv:_) = case si of
  (SpanInfo (Span (Position ls cs) (Position le ce)) ks) -> do
    let derivSpan = (ks !! ((length cons) + 1))
        firstKeySpan = (ks !! ((length cons) + 2))
    unless (ls == le) --one line
      (if ((getSpanCol (ks !! 1) == getSpanCol derivSpan) || (cs+2 == getSpanCol derivSpan)) -- alignment 'deriving'
        then
          (unless (le == getSpanLi derivSpan) --deriving one line
            (if (spanAlign (drop ((length cons) + 2) ks)) -- alignment symbols in deriving body
                then
                  (if (checkAlign getCol (getCol (getSpanInfo deriv)) derivs) -- aligment classes
                    then
                      -- indentation
                      (unless ((getSpanCol firstKeySpan == (getSpanCol derivSpan) + 2) || (getSpanLi firstKeySpan == getSpanLi derivSpan))
                                  (report (Message (Span (Position (getSpanLi derivSpan) (getSpanCol derivSpan)) (Position le ce))
                                            (colorizeKey "classes" <+> text "wrong indention")
                                            ( text "indent by 2 from"
                                            <+> colorizeKey "deriving"
                                            <+> text "write first class in same line as"
                                            <+> colorizeKey "deriving"
                                            )
                                    )
                            )
                      )
                    else
                      (report (Message (Span (Position (getSpanLi derivSpan) (getSpanCol derivSpan)) (Position le ce))
                                (colorizeKey "classes" <+> text "not aligned")
                                ( text "align derived"
                                <+> colorizeKey "classes"
                                )
                              )
                      )
                  )
                else
                  (report (Message (Span (Position (getSpanLi derivSpan) (getSpanCol derivSpan)) (Position le ce))
                            (colorizeKey "symbols" <+> text "in deriving body not aligned")
                            ( text "align"
                            <+> colorizeKey "("
                            <+> text ","
                            <+> colorizeKey ")"
                            <+> text "and"
                            <+> colorizeKey ","
                            )
                          )
                  )
            )
          )
        else
          (report (Message (Span (Position ls cs) (Position le ce))
                                  (colorizeKey "deriving" <+> text "wrong formatting")
                                  ( text "align"
                                  <+> colorizeKey "deriving"
                                  <+> text "with"
                                  <+> colorizeKey "="
                                  <+> text "or indent by 2 from"
                                  <+> colorizeKey "data"
                                  )
                  )
          )
      )
  _ -> return ()
checkDerivingC _ _ [] = return ()

-- Checks formatting for records.
checkRecord :: SpanInfo -> [ConstrDecl] -> [QualIdent] -> CSM ()
checkRecord si (cd:_) (deriv:derivs) = case (si, cd) of 
  (SpanInfo (Span (Position ls cs) (Position le ce))
            (_:_:(Span (Position l1d c1d) _):symbs@((Span (Position l1s c1s) (Position _ _)):_)),
   (RecordDecl (SpanInfo _ spans ) _ _)) -> 
              unless (ls == le) --one line
                (if (getSpanLi (last spans) == l1d) -- 'deriving' position
                  then
                    (unless (le == l1d) --deriving one line
                      (if (spanAlign symbs) -- alignment symbols of deriving
                          then
                            (if (checkAlign getCol (getCol (getSpanInfo deriv)) derivs) -- aligment classes
                              then
                                (unless ((c1s == c1d + 2) || (l1s == l1d)) -- indentation
                                            (report (Message (Span (Position l1d c1d) (Position le ce))
                                                      (colorizeKey "classes" <+> text "wrong indention")
                                                      ( text "indent by 2 from"
                                                      <+> colorizeKey "deriving"
                                                      <+> text "write first class in same line as"
                                                      <+> colorizeKey "deriving"
                                                      )
                                              )
                                      )
                                )
                              else
                                (report (Message (Span (Position l1d c1d) (Position le ce))
                                          (colorizeKey "classes" <+> text "not aligned")
                                          ( text "align derived"
                                          <+> colorizeKey "classes"
                                          )
                                        )
                                )
                            )
                          else
                            (report (Message (Span (Position l1d c1d) (Position le ce))
                                      (colorizeKey "symbols" <+> text "in deriving body not aligned")
                                      ( text "align"
                                      <+> colorizeKey "("
                                      <+> text ","
                                      <+> colorizeKey ")"
                                      <+> text "and"
                                      <+> colorizeKey ","
                                      )
                                    )
                            )
                      )
                    )
                  else
                    (report (Message (Span (Position ls cs) (Position le ce))
                                            (colorizeKey "deriving" <+> text "wrong formatting")
                                            ( text "write"
                                            <+> colorizeKey "deriving"
                                            <+> text "right of"
                                            <+> colorizeKey "}"
                                            )
                            )
                    )
                )
  _ -> return ()
checkRecord _ _  []    = return ()
checkRecord _ [] (_:_) = return ()