module Check.Src where

import Types
import Control.Monad.Trans.State 
import Control.Monad             ( when )
import Check.Src.LineLength      ( checkLineLength )
import Check.Src.Tabs            ( checkTab )
import Check.Src.TrailingSpace   ( checkTSpaces )
import Check.Src.WhiteSpaces     ( checkWSpaces )

-- TODO: iterate by yourself, for, MapM_ ChecksSrc?
-- gets sourcecode as a list of indexes and strings,
-- if a check is on, run check on source
checkSrc :: [SrcLine] -> CSM ()
checkSrc src = mapM_ (\srcl -> do conf <- getCheckList
                                  when (lineLength conf) $ checkLineLength srcl
                                  when (tab conf)        $ checkTab srcl
                                  when (trailingS conf)  $ checkTSpaces srcl
                                  when (whiteSpace conf) $ checkWSpaces srcl
                     ) src

