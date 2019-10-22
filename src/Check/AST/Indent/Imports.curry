module Check.AST.Indent.Imports where

import Curry.SpanInfo
import Curry.Span
import Curry.Position
import Curry.Types
import Text.Pretty

import Types

-- checking module
checkImports :: Module a -> Int -> CSM ()
checkImports e _ =
  case e of
    (Module _ _ _ _ imps _)   -> checkImports' imps
    _                         -> return ()

-- if not qualified and no imports are specified, warn
checkImports' :: [ImportDecl] -> CSM ()
checkImports' [] = return ()
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
  otherwise                                      ->
      checkImports' is
