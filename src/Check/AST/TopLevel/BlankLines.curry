module Check.AST.TopLevel.BlankLines where

import Text.Pretty

import Curry.SpanInfo
import Curry.Span
import Curry.Position
import Curry.Types
import Curry.Ident
import Control.Monad ( unless )

import Types

-- Applies actual check on Modules.
checkBlankLines :: Module a -> Int-> CSM ()
checkBlankLines (Module sI _ _ _ _ _ decls) _ = checkBlankLines' sI decls

-- Checks if there is at least one blank line between top level declarations.
-- If it's a type signature, there is no need to check. Otherwise the check
-- is applied.
checkBlankLines' :: SpanInfo -> [Decl a] -> CSM ()
checkBlankLines' _ []                   = return ()
checkBlankLines' _ (_:[])               = return ()
checkBlankLines' sI (decl1:decl2:decls) =
  case decl1 of
    (TypeSig _ _ _)     -> checkBlankLines' sI (decl2:decls)
    (InfixDecl _ _ _ _) -> case decl2 of
                             (InfixDecl _ _ _ _) ->  checkBlankLines' sI (decl2:decls)
                             _                   ->  do blankLine decl1 decl2
                                                        checkBlankLines' sI (decl2:decls)
    _                   -> do blankLine decl1 decl2
                              checkBlankLines' sI (decl2:decls)

-- Checks if two different top-level declarations are properly aligned
-- (This check might not work if there are comments in between).
-- 
-- Two top-level declarations are misaligned if ...
-- (a) one top level declaration is surrounded by the other (missing spatial separation),
-- (b) or there is no blank line between the two declarations.
blankLine :: Decl a -> Decl a -> CSM ()
blankLine decl1 decl2 = do
  let sI1@(SpanInfo (Span _ (Position l _)) _) = getSpanInfo decl1
      sI2                                      = getSpanInfo decl2
  unless ((getLi sI2) - l > 1) $ 
          if getEndLi sI2 < getEndLi sI1 
            then report (Message
                          (getSpan sI2)
                          (text "Top level declaration is surrounded by another declaration")
                          (text "Move inner declaration below the surrounding declaration," 
                           <+> 
                           text "keeping at least one blank line" )
                        )
            else report (Message
                          (getSpan sI2)
                          ( colorizeKey "Blank Line" <+> text "missing")
                          ( text "leave a"
                            <+> colorizeKey "blank line"
                            <+> text "between"
                            <+> colorizeKey "top level declarations")
                        )

-- Not used yet, checks for "trailing blanklines".
--
-- Because the curry frontend does not account for 
-- trailing blank lines in module spans,
-- this check seems to be unnecessary and should removed in the future.
noBlank :: SpanInfo -> Decl a -> CSM ()
noBlank sI decl = do
  let sID = getSpanInfo decl
  unless (getEndLi sI == getEndLi sID)
          (report (Message
                    (getSpan sI)
                    ( colorizeKey "blank line(s)"
                      <+> text "at the end of"
                      <+> colorizeKey "module"
                    )
                    (text "delete superfluous blank line(s) at end of module" )
                  )
          )

-- Returns line of end position from Spaninfo.
getEndLi :: SpanInfo -> Int
getEndLi si = case si of 
  (SpanInfo (Span _ (Position l _)) _) -> l
  _ -> error "getEndLi: NoSpanInfo"
