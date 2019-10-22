module Check where

import Types
import Check.Src       (checkSrc)
import Check.AST       (checkAST)
import Pretty.ToString (renderMessagesToString)

import Curry.Types

import State
import Sort (quickSort)

-- execute CSM by checking both checkAST and checkSrc, get the messages and
-- render them
checkAll :: [SrcLine] -> Module () -> Config -> String -> (Config -> String -> [SrcLine] -> [Message] -> a) -> a
checkAll src ast conf name f =
 f conf name src $ quickSort $ messages $ execState (checking src ast) (CheckState name conf [])
 where
  checking :: [SrcLine] -> Module () -> CSM ()
  checking src ast = do checkSrc src
                        checkAST ast
