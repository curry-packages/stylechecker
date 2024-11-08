module Check.AST.Indent.Class where

import Data.List ( last )

import Curry.SpanInfo
import Curry.Span
import Curry.Position
import Curry.Types
import Curry.Ident
import Text.Pretty

import Types

import Control.Monad ( unless )

-- Applies actual check on class declarations.
checkClass :: Decl a -> Int -> CSM ()
checkClass e i =
  case e of
    (ClassDecl sI _ _ _ _ _ decl ) -> checkClass' sI decl i
    _                              -> return ()

-- Checks formation of class declarations.
checkClass' :: SpanInfo -> [Decl a] -> Int -> CSM ()
checkClass' sp [] _ = case sp of 
  (SpanInfo (Span (Position l1 c1) _ ) sps) -> do
    let (Span _ (Position lw2 cw2)) = last sps
    unless (l1 == lw2) -- line position of keywords class and where 
            (report (Message (Span (Position l1 c1) (Position lw2 cw2)) 
                  (colorizeKey "class declaration" <+> text "wrong formatting")
                  ( text "write"
                  <+> colorizeKey "class"
                  <+> text "till"
                  <+> colorizeKey "where"
                  <+> text "in one line"
                  )
                )
            )
  _ ->  return ()

checkClass' sp (decl:decls) i = case sp of 
  (SpanInfo wholeSp@(Span (Position l1 c1) _ ) sps) -> do
    let (Span _ (Position lw2 cw2)) = last sps
    if (l1 == lw2) -- line position of keywords class and where
      then
        --alignment of declarations and signatures of a class
        (if (checkAlign (getCol) (getCol (getSpanInfo decl)) decls)
          then
            (unless (i+2 == getCol (getSpanInfo decl)) -- indentation
                      (report (Message wholeSp
                        (colorizeKey "class body" <+> text "wrong identation")
                        ( text "indent by 2 from"
                        <+> colorizeKey "class declaration"
                        )
                      )
            )
            )
          else
            (report (Message wholeSp
                        (colorizeKey "class body" <+> text "not aligned")
                        ( text "align"
                        <+> colorizeKey "class body"
                        )
                      )
            )
        )
      else
        (report (Message (Span (Position l1 c1) (Position lw2 cw2))
                  (colorizeKey "class declaration" <+> text "wrong formatting")
                  ( text "write"
                  <+> colorizeKey "class"
                  <+> text "till"
                  <+> colorizeKey "where"
                  <+> text "in one line"
                  )
                )
        )
  _ -> return ()