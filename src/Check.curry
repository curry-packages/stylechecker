module Check where

import Types
import Check.Src       ( checkSrc )
import Check.AST       ( checkAST )
import Pretty.ToString ( renderMessagesToString )

import Curry.Types

import Control.Monad.Trans.State 
import Data.List                 ( sort )

-- Executes CSM by checking both `checkAST` and `checkSrc`, gets the messages and
-- renders them.
checkAll :: [SrcLine] -> Module () -> Config -> String -> (Config -> String -> [SrcLine] -> [Message] -> a) -> a
checkAll src ast conf name f =
 f conf name src $ sort $ messages $ execState checking (CheckState name conf [])
 where
  checking :: CSM ()
  checking = do checkSrc src
                checkAST ast
