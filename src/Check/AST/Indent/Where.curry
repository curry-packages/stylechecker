module Check.AST.Indent.Where where

import Control.Monad  ( unless )
import Curry.SpanInfo
import Curry.Span
import Curry.Position
import Curry.Types
import Text.Pretty

import Types

-- Checks in function for `where` declarations.
checkWhere :: Decl a -> Int -> CSM ()
checkWhere e _ =
  case e of
    (FunctionDecl sI _ _ eqs)   -> checkWhere' sI eqs
    _                           -> return ()

-- Checks for both types.
checkWhere' :: SpanInfo -> [Equation a] -> CSM ()
checkWhere' _   []       = return ()
checkWhere' fSI (eq:eqs) = case fSI of 
  (SpanInfo (Span (Position _ p) _) _) -> 
    case eq of
      (Equation _ _ _ (SimpleRhs (SpanInfo _ [sp]) _ _ decls))  -> do checkDecls p sp decls
                                                                      checkWhere' fSI eqs
      (Equation _ _ _ (GuardedRhs (SpanInfo _ [sp]) _ _ decls)) -> do checkDecls p sp decls
                                                                      checkWhere' fSI eqs
      _ -> return ()
  _ -> return ()

-- Checks, if not all in one line, indentation of `where`,
-- body indentation and `where` body alignment.
checkDecls :: Int -> Span -> [Decl a] -> CSM ()
checkDecls _ _  []           = return ()
checkDecls i sp (decl:decls) = case sp of
  (Span (Position l1 c1) eP) -> 
    unless ((getEndLi (getSpanInfo decl)) == l1) $
      do
        unless (c1==(i+1))
          (report (Message (Span (Position l1 c1) eP)
                    (colorizeKey "where" <+> text "wrong indention")
                    ( text "indent by 1 space"
                      <+> text "from"
                      <+> colorizeKey "function start"
                    )
                )
          )
        unless ((getCol (getSpanInfo decl)) == (i+2))
          (report (Message (getSpan decl)
                    (colorizeKey "where" <+> text "body wrong indention")
                    ( text "indent by 1 space"
                      <+> text "from"
                      <+> colorizeKey "where"
                    )
                  )
          )
        unless (checkAlign getCol (getCol (getSpanInfo decl)) decls)
          (report (Message (getSpan decl)
                    (colorizeKey "where" <+> text "body not aligned")
                    ( text "align"
                      <+> colorizeKey "where"
                      <+> text "body"
                    )
                  )
                  )
  _ -> return ()
