module Check.Src.LineLength where

import Curry.Position
import Curry.Span
import Text.Pretty

import Types
import Control.Monad.Trans.State
import Control.Applicative       ( when )

checkLineLength :: SrcLine -> CSM ()
checkLineLength (n,l) = do
  c <- getConfig
  when (length l > (maxLineLength c))
       (report (Message
                 (Span 
                   (Position n 1)
                   (Position n (length l)))
                 (text "line too long")
                 (text "line should be under"
                 <+> colorizeKey (show (maxLineLength c))
                 <+> text "character(s)")
               )
       )
