module Pretty.ToString where

import Data.List ( intercalate )
import Data.Char ( isSpace )

import Curry.Types
import Curry.Position
import Curry.Span
import Text.Pretty

import Types

import Prelude hiding ( empty )

-- Takes a list of messages and transforms into a single String for output.
renderMessagesToString :: Config -> String -> [SrcLine] -> [Message] -> String
renderMessagesToString conf name src = intercalate "\n\n" . map (toString conf name src)

-- Renders a single message in from of:
-- Startline, Starcolumn ; Endline, Endcolumn
-- Warning:
-- <warnmessage>
-- Hint_
-- <hintmessage>
toString :: Config -> String -> [SrcLine] -> Message -> String
toString conf name src m = case m of 
  (Message (Span pS pE) sW sH) ->
    pPrint (bold ( text name <> text ":" <+> posToDoc pS
            <> text "-" <> posToDoc pE <> text ":" <+> warningToDoc sW
            <$$> hintToDoc conf sH)
            <$$> if (code conf && (verbosity conf) >= 1)
                    then getCodeDoc src (Span pS pE)
                    else empty
          )
  _ -> error "toString: Invalid Span"

-- Return `Doc` with formatted Warning.
warningToDoc :: Doc -> Doc
warningToDoc sW = yellow (text "Warning") <> text ":"
                     <+> sW

-- Return `Doc` with formatted Hint.
hintToDoc :: Config -> Doc -> Doc
hintToDoc conf sH = if (hints conf && (verbosity conf) >= 1)
                      then text "Hint:" <+> sH
                      else empty

-- Renders a `Position` to `Doc`.
posToDoc :: Position -> Doc
posToDoc (Position line column) =
  text ((show line) ++ ":" ++ (show column))
posToDoc NoPos = error "posToDoc: NoPos"

--- Colors a `Doc` in cyan.
lineColor :: (Doc -> Doc)
lineColor = cyan

-- Returns corresponding code in `Doc`.
getCodeDoc :: [SrcLine] -> Span -> Doc
getCodeDoc (l:ls) sp = case sp of 
  (Span (Position l1 c1) (Position l2 c2))
    | fst l < l1
      -> getCodeDoc ls sp
    | fst l > l2
      -> empty
    | fst l == l1 && fst l == l2
      -> getLineNumDoc (l)
         <+> getLineDoc 1 (\n -> n >= c1 && n <= c2) (snd l)
         <$$> createUnderLineNumDoc l
         <> createUnderLineDoc' 1 (\n -> n >= c1 && n <= c2) (snd l)
         <$$> getCodeDoc ls sp
    | fst l == l1
      -> getLineNumDoc (l)
         <+> getLineDoc 1 ((<=) c1) (snd l)
         <$$> createUnderLineNumDoc l
         <> createUnderLineDoc 1 ((<=) c1) False (snd l)
         <$$> getCodeDoc ls sp
    | fst l == l2
      -> getLineNumDoc (l)
         <+> getLineDoc 1 ((>=) c2) (snd l)
         <$$> createUnderLineNumDoc l
         <> createUnderLineDoc 1 ((>=) c2) False (snd l)
         <$$> getCodeDoc ls sp
    | otherwise
      -> getLineNumDoc (l)
         <+> red (text (snd l))
         <$$> createUnderLineNumDoc l
         <> createUnderLineDoc 1 (\_ -> True) False (snd l)
         <$$> getCodeDoc ls sp
  _ -> error "getCodeDoc: Invalid Span"
getCodeDoc [] _ = empty

-- Creates 0|
--         1|
--         2|
-- in front of code excerpt.
getLineNumDoc :: SrcLine -> Doc
getLineNumDoc l = lineColor $ empty <+> text (show (fst l)) <+> text "|"

-- Same as `getLineNumDoc` but for the underline line.
createUnderLineNumDoc :: SrcLine -> Doc
createUnderLineNumDoc l = lineColor $ text ((replicate (length (show (fst l))) ' ') ++ " | ")

-- Retrieves code excerpt and marks parts red by given condition.
getLineDoc :: Int -> (Int -> Bool) -> String -> Doc
getLineDoc n f (s:ss)
  | f n              = red (text ([s])) <> getLineDoc (n+1) f ss
  | otherwise        = text [s] <> getLineDoc (n+1) f ss
getLineDoc _ _ []    = empty

-- Underlines part of corresponding code which are marked in red.
createUnderLineDoc :: Int -> (Int -> Bool) -> Bool -> String -> Doc
createUnderLineDoc n f b (s:ss)
  | not (isSpace s) && not b && f n       = red (text "^") <>  createUnderLineDoc (n+1) f (not b) ss
  | isSpace s && not b                    = text [s] <>  createUnderLineDoc (n+1) f  b ss
  | f n                                   = red (text "^") <> createUnderLineDoc (n+1) f b ss
  | otherwise                             = space <> createUnderLineDoc (n+1) f b ss
createUnderLineDoc _ _ _ []               = empty

-- Underlining for the case, that the warning area is in one line.
createUnderLineDoc' :: Int -> (Int -> Bool) -> String -> Doc
createUnderLineDoc' n f (_:ss)
  | f n                                   = red (text "^") <> createUnderLineDoc' (n+1) f ss
  | otherwise                             = space <> createUnderLineDoc' (n+1) f ss
createUnderLineDoc' _ _ []                = empty