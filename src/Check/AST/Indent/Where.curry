module Check.AST.Indent.Where where

import Curry.SpanInfo
import Curry.Span
import Curry.Position
import Curry.Types
import Text.Pretty

import Types

-- check in function for where declarations
checkWhere :: Decl a -> Int -> CSM ()
checkWhere e i =
  case e of
    (FunctionDecl sI _ _ eqs)   -> checkWhere' sI eqs
    _                           -> return ()

-- check for both types
checkWhere' :: SpanInfo -> [Equation a] -> CSM ()
checkWhere' _                                         []       = return ()
checkWhere' fSI@(SpanInfo (Span (Position _ p) eP) _) (eq:eqs) =
  case eq of
    (Equation _ _ (SimpleRhs (SpanInfo _ [sp]) _ decls))  -> do checkDecls p sp decls
                                                                checkWhere' fSI eqs
    (Equation _ _ (GuardedRhs (SpanInfo _ [sp]) _ decls)) -> do checkDecls p sp decls
                                                                checkWhere' fSI eqs
    _                                                     -> return ()

-- check, if not all in one line, indentation of where,
-- body indentation and and where body alignment
checkDecls :: Int -> Span -> [Decl a] -> CSM ()
checkDecls _ _                          []           = return ()
checkDecls i (Span (Position l1 c1) eP) (decl:decls) =
  unlessM ((getEndLi (getSpanInfo decl)) == l1)
    (do
       unlessM (c1==(i+1))
         (report (Message (Span (Position l1 c1) eP)
                   (colorizeKey "where" <+> text "wrong indention")
                   ( text "indent by 1 space"
                     <+> text "from"
                     <+> colorizeKey "function start"
                   )
               )
         )
       unlessM ((getCol (getSpanInfo decl)) == (i+2))
         (report (Message (getSpan decl)
                   (colorizeKey "where" <+> text "body wrong indention")
                   ( text "indent by 1 space"
                     <+> text "from"
                     <+> colorizeKey "where"
                   )
                 )
         )
       unlessM (checkAlign getCol (getCol (getSpanInfo decl)) decls)
         (report (Message (getSpan decl)
                   (colorizeKey "where" <+> text "body not aligned")
                   ( text "align"
                     <+> colorizeKey "where"
                     <+> text "body"
                   )
                 )
                 )
      )
