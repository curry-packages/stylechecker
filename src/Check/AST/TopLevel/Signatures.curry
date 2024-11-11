module Check.AST.TopLevel.Signatures where

import Text.Pretty

import Curry.SpanInfo
import Curry.Span
import Curry.Position
import Curry.Types
import Curry.Ident
import Control.Monad ( unless )

import Types

-- Applies actual check on `Module`.
checkTopLevelSig :: Module a -> Int -> CSM ()
checkTopLevelSig (Module _ _ _ _ _ _ decls) _ = checkTopLevelSig' decls decls

-- Checks for each declaration if they have a corresponding signature.
-- If it is not a declaration (typesig), ignore.
checkTopLevelSig' :: [Decl a] -> [Decl a] -> CSM ()
checkTopLevelSig' []           _        = return ()
checkTopLevelSig' (decl:decls) allDecls =
  case decl of
    (FunctionDecl
      (SpanInfo s _)
      _ _ _) ->
      do
        pair <- checkPair decl allDecls
        unless (pair)
          ( report (Message
                       s
                       ( colorizeKey "type signature"
                         <+> text "missing")
                       ( colorizeKey "top level functions"
                         <+> text "should have"
                         <+> colorizeKey "type signatures"
                       )
                     )
          )
        checkTopLevelSig' decls allDecls
    _        ->
      checkTopLevelSig' decls allDecls

-- Checks if Decl has a signature in the list of decls of module,
-- by goung through each declaration in module,
-- if so, returns true and checks the position.
checkPair :: Decl a -> [Decl a] -> CSM Bool
checkPair _ []           = return False
checkPair d (decl:decls) = case d of
  fD@(FunctionDecl sIF _ (Ident _ s _) _) -> case decl of
    (TypeSig sIT i _) -> if (s == getIdentS (head i))
                           then do checkPos sIF sIT s
                                   return True
                           else checkPair fD decls
    _                 -> checkPair fD decls
  _ -> return False

-- Checks if from two spaninfos, whether or not one construct is ending
-- a line above where the second one starts.
checkPos :: SpanInfo -> SpanInfo -> String -> CSM ()
checkPos s1 s2 s = case (s1, s2) of 
  ((SpanInfo (Span (Position lF cF) eP) _), 
   (SpanInfo (Span _  (Position lT _)) _)) -> 
    unless (lT == (lF-1))
    (report (Message
              (Span (Position lF cF) eP)
              ( colorizeKey "type signature" <+> text "wrong position")
              ( colorizeKey "type signature"
                <+> text "of"
                <+> colorizeKey s
                <+> text "should be placed directly above its"
                <+> colorizeKey "function declaration"
              )
              )
    )
  _ -> return ()

-- Extracts string (name) from Indent.
getIdentS :: Ident -> String
getIdentS (Ident _ s _) = s
