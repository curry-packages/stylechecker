module Check.AST.Indent.Class where

import List (last)

import Curry.SpanInfo
import Curry.Span
import Curry.Position
import Curry.Types
import Curry.Ident
import Text.Pretty

import Types

-- applies actual check on class constructs
checkClass :: Decl a -> Int -> CSM ()
checkClass e i =
  case e of
    --(ClassDecl sI _ _ _ decl ) -> checkClass' sI decl i
    _                          -> return ()

-- checks formation of class declarations
checkClass' :: SpanInfo -> [Decl a] -> Int -> CSM ()
checkClass' (SpanInfo wholeSp@(Span (Position l1 c1) _ ) sps)
            []
            i = do
  let (Span _ (Position lw2 cw2)) = last sps
  unlessM (l1 == lw2) -- line position of keywords class and where
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
checkClass' (SpanInfo wholeSp@(Span (Position l1 c1) _ ) sps)
            (decl:decls)
            i = do
  let (Span _ (Position lw2 cw2)) = last sps
  if (l1 == lw2) -- line position of keywords class and where
    then
      --alignment of declarations and signatures of a class
      (if (checkAlign (getCol) (getCol (getSpanInfo decl)) decls)
        then
           (unlessM (i+2 == getCol (getSpanInfo decl)) -- indentation
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

