module Pretty.ToJson where

import Data.List  ( intercalate )
import Data.Char  ( isSpace )

import Curry.Types
import Curry.Position
import Curry.Span
import Text.Pretty
import JSON.Data
import JSON.Pretty

import Types

import Prelude hiding ( empty )

-- Takes a list of messages and transforms into a single Json-Object for output.
renderMessagesToJson :: Config -> String -> [SrcLine] -> [Message] -> String
renderMessagesToJson conf name src ms = ppJSON $ JArray $ map (toJson conf name src) ms

-- Renders a single message with fields:
-- [ ("file" : string),
--   ("span" :
--     ("from" :
--       [("line" : number), ("column" : number)])
--     ("to" :
--       [("line" : number), ("column" : number)])),
--   ("warning" : string),
--   ("hint" : string)
-- ]
toJson :: Config -> String -> [SrcLine] -> Message -> JValue
toJson conf name _ m = case m of 
  (Message (Span (Position l1 c1) (Position l2 c2)) sW sH) ->
    JObject [ ("file", JString name)
            , ("span", JObject [ ("from", JObject [ ("line" ,  JNumber (toFloat l1))
                                                  , ("column", JNumber (toFloat c1))])
                              , ("to", JObject [ ("line" ,  JNumber (toFloat l2))
                                                , ("column", JNumber (toFloat c2))])
                              ])
            , ("warning", JString (pPrint (warningToDoc sW)))
            ,("hint", JString (pPrint (hintToDoc conf sH)))
            ]
  _ -> error "toJson: Invalid Span"

-- Returns Doc with formatted warning.
warningToDoc :: Doc -> Doc
warningToDoc sW = text "Warning" <> text ":"
                     <+> sW

-- Return Doc with formatted hint.
hintToDoc :: Config -> Doc -> Doc
hintToDoc conf sH = if (hints conf)
                         then text "Hint"
                              <> text ":"
                              <+> sH
                              else empty
