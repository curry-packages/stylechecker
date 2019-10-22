module Check.AST.Indent.Header where

import List (last)

import Curry.SpanInfo
import Curry.Span
import Curry.Position
import Curry.Types
import Curry.Ident
import Text.Pretty

import Types

-- applies actual check on modules
checkModuleHeader :: Module a -> Int -> CSM ()
checkModuleHeader e i =
  case e of
    (Module sI _ (ModuleIdent sII _) (Just exports) _ _) -> checkModuleHeader' sI sII exports i
    _                -> return ()

-- check formatting of module header
checkModuleHeader' :: SpanInfo -> SpanInfo -> ExportSpec -> Int -> CSM ()
checkModuleHeader' (SpanInfo _ ((Span start@(Position modL modC) _):(Span _ end@(Position wheL _):_))) spanInfoIdent exp i =
  unlessM (modL == wheL) -- one line
    (if (checkExportLines exp) -- aligment exports lines
       then (if (not (modL == (getLi (getSpanInfo exp))) && (getLi spanInfoIdent == modL)) -- 'module' and module name in one line
               then
                 (if wheL == (getEndLi (getSpanInfo exp)) -- where formatting
                    then (unlessM ((getCol (getSpanInfo exp)) == (modC + 2)) -- exports indentation
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

 --check if lines are aligned
checkExportLines :: ExportSpec -> Bool
checkExportLines (Exporting (SpanInfo _ sp@((Span (Position _ c) _):ss)) _) = checkAlign $ c:(getListOfNewLineStart(sp))
 where
  checkAlign :: [Int] -> Bool
  checkAlign (_:[]) = True
  checkAlign (i1:i2:is) = (i1==i2) && (checkAlign (i2:is))

-- get list of line column start position
getListOfNewLineStart :: [Span] -> [Int]
getListOfNewLineStart (_:[]) = []
getListOfNewLineStart ((Span (Position l1 _) _):s2@(Span (Position l2 c2) _):sps)
  | l1 == l2  = getListOfNewLineStart (s2:sps)
  | otherwise = c2 : (getListOfNewLineStart (s2:sps))
