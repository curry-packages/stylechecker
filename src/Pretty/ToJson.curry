module Pretty.ToJson where

import List (intercalate)
import Char (isSpace)
import Float (i2f)

import Curry.Types
import Curry.Position
import Curry.Span
import Text.Pretty
import JSON.Data
import JSON.Pretty

import Types

-- takes a list of messages and transforms into a single Json-Object for output
renderMessagesToJson :: Config -> String -> [SrcLine] -> [Message] -> String
renderMessagesToJson conf name src ms = ppJSON $ JArray $ map (toJson conf name src) ms

-- renders a single message with fields:
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
toJson conf name src (Message (Span (Position l1 c1) (Position l2 c2)) sW sH) =
  JObject [ ("file", JString name)
          , ("span", JObject [ ("from", JObject [ ("line" , JNumber (i2f l1))
                                                , ("column", JNumber (i2f c1))])
                             , ("to", JObject [ ("line" , JNumber (i2f l2))
                                              , ("column", JNumber (i2f c2))])
                             ])
          , ("warning", JString (pPrint (warningToDoc sW)))
          ,("hint", JString (pPrint (hintToDoc conf sH)))
          ]

--return Doc with formatted Warning
warningToDoc :: Doc -> Doc
warningToDoc sW = text "Warning" <> text ":"
                     <+> sW

--return Doc with formatted Hint
hintToDoc :: Config -> Doc -> Doc
hintToDoc conf sH = if (hints conf)
                         then text "Hint"
                              <> text ":"
                              <+> sH
                              else empty


