module Check.Src.TrailingSpace where

import List      (last)

import Curry.Position
import Curry.Span
import Text.Pretty

import Types
import State

--Check for each line, if the last character is a space, in this case get the
--number of them (by reversing the line) and report
checkTSpaces :: SrcLine -> CSM ()
checkTSpaces (n,l) = do
  whenM ((last l) == ' ')
        (do let endPos = length l
                startPos = endPos - (length (takeWhile (\ x -> x == ' ') ( reverse l))) + 1
                posDif = endPos - startPos
                sp = "space" ++ (if posDif > 1 then "s" else "")
            (report (Message
                      (Span
                        (Position n startPos)
                        (Position n endPos))
                      ( colorizeKey (show (endPos - startPos))
                        <+> text "trailing"
                        <+> colorizeKey sp
                        <+> text "found")
                      ( text "delete"
                        <+> colorizeKey sp
                        <+> text "at end of line"))))
