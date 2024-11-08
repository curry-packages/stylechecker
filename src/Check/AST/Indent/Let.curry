module Check.AST.Indent.Let where

import Control.Monad  ( unless )
import Curry.SpanInfo
import Curry.Span
import Curry.Position
import Curry.Types
import Text.Pretty

import Types

-- applies actual check on let constructs
checkLet :: Expression a -> Int -> CSM ()
checkLet e i =
  case e of
    (Let sI _ dcls expr) -> checkLet' sI dcls expr i
    _                    -> return ()

-- Checks indention of in expression first,
-- then checks if 
--  (a) `let` and `in` are in one line.
--  (b) they are aligned (indented) properly and the `let` declarations are aligned.
checkLet' :: SpanInfo -> [Decl a] -> Expression a -> Int -> CSM ()
checkLet' si (dcl:dcls) expr i = case si of 
  (SpanInfo
            (Span (Position l1 c1) (Position l2 c2))
            [Span (Position _  cl) _, 
             Span (Position li ci) _]
          ) -> do
    checkExprIndent expr li ci i
    unless (l1 == l2) $
      do
        twoLines (Span (Position l1 c1) (Position l2 c2)) cl ci
        if checkLetDeclarations (getCol (getSpanInfo dcl)) dcls
          then declarationIndent (Span (Position l1 c1) (Position l2 c2)) dcl i
          else report (Message
                        (Span (Position l1 c1) (Position l2 c2))
                        ( colorizeKey "let"
                          <+> text "declarations not aligned"
                        )
                        ( text "align"
                          <+> colorizeKey "let"
                          <+> text "declarations")
                      )
  _ -> return ()
checkLet' _ [] _ _ = return ()

-- Returns true if list of declarations are aligned with inputed position of first decl.
checkLetDeclarations :: Int -> [Decl a] -> Bool
checkLetDeclarations p (dcl:dcls@(_:_)) =
  (p == getCol (getSpanInfo dcl)) && checkLetDeclarations (getCol (getSpanInfo dcl)) dcls
checkLetDeclarations p [dcl]            = (p == getCol (getSpanInfo dcl))
checkLetDeclarations _ []               = True

-- Compares column positions of `let` and `in`.
twoLines :: Span -> Int -> Int -> CSM ()
twoLines sp x y =
  unless (x == y) $
    report (Message
              sp
              ( colorizeKey "let"
                <+> text "and"
                <+> colorizeKey "in"
                <+> text "not aligned"
              )
              ( text "align"
                <+> colorizeKey "let"
                <+> text "and"
                <+> colorizeKey "in"
              )
            )

-- `let` declarations should either be indented by 2 from let
-- or indentation edge.
declarationIndent :: Span -> Decl a -> Int -> CSM ()
declarationIndent sp dcl i =
  let sI = (getSpanInfo dcl)
      l = getLi (SpanInfo sp [])
      c = getCol (SpanInfo sp [])
  in
    unless ((getLi sI) == l)
      (unless ((getCol sI) == (c+2)||(getCol sI == (i+2))) $
        report (Message
                  sp
                  ( colorizeKey "let"
                    <+> text "declarations not properly indented"
                  )
                  ( text "indent by 2 from"
                    <+> colorizeKey "let"
                    <+> text "or"
                    <+> colorizeKey "outer block"
                  )
                )
      )

-- Checks if in expression is on the same line as `in`, if not, check if indented
-- by 2 from edge or `in`.
checkExprIndent :: Expression a -> Int -> Int -> Int -> CSM ()
checkExprIndent expr li ci i =
  do let sI = getSpanInfo expr
     unless (li == (getLi sI))
             (unless (((ci+2) == (getCol sI))||((i+2)==(getCol sI)))
                      (report (Message
                                (getSpan sI)
                                ( colorizeKey "in Expression"
                                  <+> text "not properly indented"
                                )
                                ( colorizeKey "in Expression"
                                  <+> text "should be indented by 2 from"
                                  <+> colorizeKey "in"
                                  <+> text "or"
                                  <+> colorizeKey "outer block"
                                )
                              )
                      )
             )
