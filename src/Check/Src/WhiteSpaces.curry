module Check.Src.WhiteSpaces where

import Data.Char ( isSpace )

import Curry.Position
import Curry.Span
import Text.Pretty

import Types
import Control.Monad.Trans.State
import Control.Applicative       ( when )

checkWSpaces :: SrcLine -> CSM ()
checkWSpaces (n, l) = checkWSpaces' l n 0

checkWSpaces' :: String -> Int -> Int -> CSM ()
checkWSpaces' []     _ _ = return ()
checkWSpaces' (x:xs) l c = do
  when (isWhiteSpace x)
    (report (Message
      (Span 
        (Position l c)
        (Position l (c+1)))
      ( colorizeKey "Whitespace aside of spaces, linebreaks and tabs found"
        <+> text "found")
      ( text "delete"
        <+> colorizeKey "Whitespaces")))
  checkWSpaces' xs l (c+1)

isWhiteSpace :: Char -> Bool
isWhiteSpace x | x == ' '         = False
               | x == '\t'        = False
               | x == '\n'        = False
               | otherwise  = isSpace x

