module Check.AST.TopLevel.BlankLines where

import Text.Pretty

import Curry.SpanInfo
import Curry.Span
import Curry.Position
import Curry.Types
import Curry.Ident

import Types

-- applies actual check on Modules
checkBlankLines :: Module a -> Int-> CSM ()
checkBlankLines e _ =
  case e of
    (Module sI _ _ _ _ decls) -> checkBlankLines' sI decls
    _                         -> return ()

--checks if there is at least one blank line between TopLevel Declarations,
--case its a typesignature, there is no need to check, else check
checkBlankLines' :: SpanInfo -> [Decl a] -> CSM ()
checkBlankLines' _ []                   = return ()
checkBlankLines' sI (decl:[])           = return () --noBlank sI decl
checkBlankLines' sI (decl1:decl2:decls) =
  case decl1 of
    (TypeSig _ _ _)     -> checkBlankLines' sI (decl2:decls)
    (InfixDecl _ _ _ _) -> case decl2 of
                           (InfixDecl _ _ _ _) ->  checkBlankLines' sI (decl2:decls)
                           _                   ->  do blankLine decl1 decl2
                                                      checkBlankLines' sI (decl2:decls)
    _                   -> do blankLine decl1 decl2
                              checkBlankLines' sI (decl2:decls)

-- see if the difference between start and end of the two declarations is at
-- least one (This check doesn't work if there are comments in between)
blankLine :: Decl a -> Decl a -> CSM ()
blankLine decl1 decl2 = do
  let sI1@(SpanInfo (Span _ (Position l c)) _) = getSpanInfo decl1
      sI2 = getSpanInfo decl2
  unlessM ((getLi sI2) - l > 1)
          (report (Message
                    (getSpan sI2)
                    ( colorizeKey "Blank Line" <+> text "missing")
                    ( text "leave a"
                      <+> colorizeKey "blank line"
                      <+> text "between"
                      <+> colorizeKey "top level declarations")
                  )
          )

-- not used yet, checking for "trailing blanklines"
noBlank :: SpanInfo -> Decl a -> CSM ()
noBlank sI decl = do
  let sID = getSpanInfo decl
  unlessM ((getEndLi sI) == (getEndLi sID))
          (report (Message
                    (getSpan sI)
                    ( colorizeKey "blank line(s)"
                      <+> text "at the end of"
                      <+> colorizeKey "module"
                    )
                    (text "delete superfluous blank line(s) at end of module" )
                  )
          )

-- returns line of end position from Spaninfo
getEndLi :: SpanInfo -> Int
getEndLi (SpanInfo (Span _ (Position l _)) _) = l
