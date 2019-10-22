module Check.Src where

import Types
import State
import Check.Src.LineLength      (checkLineLength)
import Check.Src.Tabs            (checkTab)
import Check.Src.TrailingSpace   (checkTSpaces)
import Check.Src.WhiteSpaces     (checkWSpaces)

--TODO: iterate by yourself, for, MapM_ ChecksSrc?
--gets sourcecode as a list of indexes and strings,
--if a check is on, run check on source
checkSrc :: [SrcLine] -> CSM ()
checkSrc src = mapM_ (\srcl -> do conf <- getCheckList
                                  whenM (lineLength conf) $ checkLineLength srcl
                                  whenM (tab conf) $ checkTab srcl
                                  whenM (trailingS conf) $ checkTSpaces srcl
                                  whenM (whiteSpace conf) $ checkWSpaces srcl
                     ) src

