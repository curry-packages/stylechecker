module Check.Src.Tabs where

import Data.List

import Curry.Position
import Curry.Span
import Text.Pretty

import Types
import Control.Monad.Trans.State

checkTab :: SrcLine -> CSM ()
checkTab (n,l) = do reportTabs n (map ((+) 1) (elemIndices '\t' l))

reportTabs :: Int -> [Int] -> CSM ()
reportTabs n (i:is) = do report (Message
                                  (Span
                                    (Position n i)
                                    (Position n (i+1)))
                                  (colorizeKey "tab" <+> text "found")
                                  (text "use"
                                  <+> colorizeKey "spaces"
                                  <+> text "instead of"
                                  <+> colorizeKey "tabs")
                                )
                         reportTabs n is
reportTabs _ []     = return ()
