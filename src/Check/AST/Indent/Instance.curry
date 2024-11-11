
module Check.AST.Indent.Instance where

import Data.List     ( last )
import Control.Monad ( unless )

import Curry.SpanInfo
import Curry.Span
import Curry.Position
import Curry.Types
import Curry.Ident
import Text.Pretty

import Types

-- Applies actual check on instance declaration.
checkInstance :: Decl a -> Int -> CSM ()
checkInstance e i =
  case e of
    (InstanceDecl sI _ _ _ _ decl) -> checkInstance' sI decl i
    _                              -> return ()

-- Checks formatting of instance declaration.
checkInstance' :: SpanInfo -> [Decl a] -> Int -> CSM ()
checkInstance' si []_ = case si of 
  (SpanInfo (Span (Position l1 c1) _ ) sps) -> do
    let (Span _ (Position lw2 cw2)) = last sps
    unless (l1 == lw2) -- line position of keywords instance and where
          (report (Message (Span (Position l1 c1) (Position lw2 cw2))
                    (colorizeKey "instance declaration" <+> text "wrong formatting")
                    ( text "write"
                    <+> colorizeKey "instance"
                    <+> text "till"
                    <+> colorizeKey "where"
                    <+> text "in one line"
                    )
                  )
          )
  _ -> return ()
checkInstance' si (decl:decls) i = case si of 
  (SpanInfo wholeSp@(Span (Position l1 c1) _ ) sps) -> do
    let (Span _ (Position lw2 cw2)) = last sps
    if (l1 == lw2) -- line position of keywords instance and where
      then
        -- alignment of declarations of instance
        (if (checkAlign getCol (getCol (getSpanInfo decl)) decls)
          then
            (unless (i+2 == getCol (getSpanInfo decl)) -- indentation
                      (report (Message wholeSp
                        (colorizeKey "instance body" <+> text "wrong identation")
                        ( text "indent by 2 from"
                        <+> colorizeKey "instance declaration"
                        )
                      )
            )
            )
          else
            (report (Message wholeSp
                        (colorizeKey "instance body" <+> text "not aligned")
                        ( text "align"
                        <+> colorizeKey "instance body"
                        )
                      )
            )
        )
      else
        (report (Message (Span (Position l1 c1) (Position lw2 cw2))
                  (colorizeKey "instance declaration" <+> text "wrong formatting")
                  ( text "write"
                  <+> colorizeKey "instance"
                  <+> text "till"
                  <+> colorizeKey "where"
                  <+> text "in one line"
                  )
                )
        )
  _ -> return ()
