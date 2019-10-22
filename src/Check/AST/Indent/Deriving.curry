module Check.AST.Indent.Deriving where

import List (last)

import Curry.SpanInfo
import Curry.Span
import Curry.Position
import Curry.Types
import Curry.Ident
import Text.Pretty

import Types

-- applies actual check on Data constructs
checkDeriving :: Decl a -> Int -> CSM ()
checkDeriving e _ =
  case e of
    (DataDecl sI _ _ constr@(l:ls) derivs@(d:ds)) -> checkDeriving' sI constr derivs
    _                                             -> return ()

-- check according to type
checkDeriving' :: SpanInfo -> [ConstrDecl] -> [QualIdent] -> CSM ()
checkDeriving' sI (l:ls) d = case l of
  (ConstrDecl _ _ _ _ _)  -> checkDerivingC sI (l:ls) d
  (RecordDecl _ _ _ _ _)  -> checkRecord sI (l:ls) d
  _                       -> return ()

-- check formatting
checkDerivingC :: SpanInfo -> [ConstrDecl] -> [QualIdent]-> CSM ()
checkDerivingC (SpanInfo (Span (Position ls cs) (Position le ce)) ks) cons derivs@(deriv:_) = do
  let derivSpan = (ks !! ((length cons) + 1))
      firstKeySpan = (ks !! ((length cons) + 2))
  unlessM (ls == le) --one line
    (if ((getSpanCol (ks !! 1) == getSpanCol derivSpan) || (cs+2 == getSpanCol derivSpan)) -- alignment 'deriving'
       then
         (unlessM (le == getSpanLi derivSpan) --deriving one line
           (if (spanAlign (drop ((length cons) + 2) ks)) -- alignment symbols in deriving body
              then
                (if (checkAlign getCol (getCol (getSpanInfo deriv)) derivs) -- aligment classes
                  then
                    -- indentation
                    (unlessM ((getSpanCol firstKeySpan == (getSpanCol derivSpan) + 2) || (getSpanLi firstKeySpan == getSpanLi derivSpan))
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

-- check formatting for records
checkRecord :: SpanInfo -> [ConstrDecl] -> [QualIdent] -> CSM ()
checkRecord (SpanInfo (Span (Position ls cs) (Position le ce))
            (_:_:(Span (Position l1d c1d) _):
              symbs@((Span (Position l1s c1s) (Position _ _)):_)))
            ((RecordDecl (SpanInfo _ spans ) _ _ ident fs):_)
            (deriv:derivs) =
              unlessM (ls == le) --one line
                (if (getSpanLi (last spans) == l1d) -- 'deriving' position
                  then
                    (unlessM (le == l1d) --deriving one line
                      (if (spanAlign symbs) -- alignment symbols of deriving
                          then
                            (if (checkAlign getCol (getCol (getSpanInfo deriv)) derivs) -- aligment classes
                              then
                                (unlessM ((c1s == c1d + 2) || (l1s == l1d)) -- indentation
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
