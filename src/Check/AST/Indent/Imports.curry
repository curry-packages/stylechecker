module Check.AST.Indent.Imports where

import Curry.SpanInfo
import Curry.Span
import Curry.Position
import Curry.Types
import Text.Pretty

import Types

-- Checks all imports in a module.
checkImports :: Module a -> Int -> CSM ()
checkImports (Module _ _ _ _ _ imps _) _ = checkImports' imps

-- Emits a warning if an import is not qualified and no imports are specified.
checkImports' :: [ImportDecl] -> CSM ()
checkImports' []     = return ()
checkImports' (i:is) = case i of
  (ImportDecl (SpanInfo sp _) _ False _ Nothing) ->
    do
      report (Message (sp)
                      (colorizeKey "Imports" <+> text "should be qualified or specified")
                      ( text "use"
                       <+> colorizeKey "qualified"
                       <+> text "or explicitly state which entities are to be imported"
                      )
             )
      checkImports' is
  _ ->
      checkImports' is
