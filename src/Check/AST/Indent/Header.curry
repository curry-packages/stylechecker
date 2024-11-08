module Check.AST.Indent.Header where

import Data.List     ( last )
import Control.Monad ( unless )

import Curry.SpanInfo
import Curry.Span
import Curry.Position
import Curry.Types
import Curry.Ident
import Text.Pretty

import Types

-- Applies actual check on modules.
checkModuleHeader :: Module a -> Int -> CSM ()
checkModuleHeader e i =
  case e of
    (Module sI _ _ (ModuleIdent sII _) (Just exports) _ _) -> checkModuleHeader' sI sII exports i
    _ -> return ()

-- Check formatting of module header.
checkModuleHeader' :: SpanInfo -> SpanInfo -> ExportSpec -> Int -> CSM ()
checkModuleHeader' si spanInfoIdent exp _ = case si of
  (SpanInfo _ ((Span start@(Position modL modC) _):(Span _ end@(Position wheL _):_))) -> 
    unless (modL == wheL) -- one line
      (if (checkExportLines exp) -- aligment exports lines
        then (if (not (modL == (getLi (getSpanInfo exp))) && (getLi spanInfoIdent == modL)) -- 'module' and module name in one line
                then
                  (if wheL == (getEndLi (getSpanInfo exp)) -- where formatting
                      then (unless ((getCol (getSpanInfo exp)) == (modC + 2)) -- exports indentation
                              (report (Message (Span start end)
                                              (colorizeKey "module exports" <+> text "wrong indentation")
                                              ( text "indent by 2 from"
                                                <+> colorizeKey "module <modulename>")
                                      )
                              )
                          )
                      else
                        (report (Message (Span start end)
                                          (colorizeKey "where" <+> text "wrong formatting")
                                          ( text "write keyword"
                                          <+> colorizeKey "where"
                                          <+> text "and last line of exports in one line" )
                                  )
                        )
                  )
                else
                  (report (Message (Span start end)
                                    (colorizeKey "module <modulename> (...)" <+> text "wrong formatting")
                                    ( text "if not fitting header in one line: break after"
                                    <+> colorizeKey "module <modulename>"
                                    <> text ", write keyword"
                                    <+> colorizeKey "module"
                                    <+> text "and name of module in one line" )
                          )
                  )
              )
        else
          (report (Message (Span start end)
                            (colorizeKey "module exports" <+> text "wrong alignment")
                            ( text "start lines with commata or braces, align these" )
                    )
          )
      )
  _ -> return ()

 -- Checks if lines are aligned.
checkExportLines :: ExportSpec -> Bool
checkExportLines es = case es of 
  (Exporting (SpanInfo _ sp@((Span (Position _ c) _):_)) _) -> checkAlign $ c:(getListOfNewLineStart(sp))
  _ -> False
 where
  checkAlign :: [Int] -> Bool
  checkAlign []         = True
  checkAlign (_:[])     = True
  checkAlign (i1:i2:is) = (i1==i2) && (checkAlign (i2:is))

-- Gets list of line column start position.
getListOfNewLineStart :: [Span] -> [Int]
getListOfNewLineStart []     = []
getListOfNewLineStart (_:[]) = []
getListOfNewLineStart (s1:s2:sps) = case (s1, s2) of 
  ((Span (Position l1 _) _), (Span (Position l2 c2) _)) | l1 == l2  -> getListOfNewLineStart (s2:sps)
                                                        | otherwise -> c2 : (getListOfNewLineStart (s2:sps))
  _ -> []