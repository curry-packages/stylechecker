module Parse.Config where

import List             (isPrefixOf, isSuffixOf, last)
import Read             (readInt)

import Pretty.ToJson    (renderMessagesToJson)
import Pretty.ToString  (renderMessagesToString)
import Types

--defaultConfig, checks everything, maxLineLength is 80
defaultConfig :: Config
defaultConfig = Config (CheckList
                          True True True True
                          True True True True
                          True True True True
                          True True True True
                          True True True True
                          True True True True
                          True True True True
                          True
                          )
                          TEXT 1 True True 80

-- parseConfig returns the config in form of a record of bools,
-- by parsing the config file, initiate with defaultConfig
parseConfig :: Bool -> String -> IO Config
parseConfig verb conf = do
  when verb $ putStrLn $ "INFO: Reading config file '" ++ conf ++ "'..."
  file <- readFile conf
  let ls = lines file
  return $ Config (readCheckList ls (checks defaultConfig))
                  (readOType ls)
                  (readVerbosity ls (verbosity defaultConfig))
                  (readHint      ls)
                  (readCode      ls)
                  (readLength    ls (maxLineLength defaultConfig))

--parseChecklist, if a line starts with corresponding option,
--if check is 0, turn off check (since default is on)
readCheckList :: [String] -> CheckList -> CheckList
readCheckList [] checkl
  = checkl
readCheckList (l:ls) checkl
  | isPrefixOf "lineLength" l && isSuffixOf "0" l
  = readCheckList ls $ checkl { lineLength = False }
  | isPrefixOf "tabs" l && isSuffixOf "0" l
  = readCheckList ls $ checkl {tab = False}
  | isPrefixOf "ifThenElse" l && isSuffixOf "0" l
  = readCheckList ls $ checkl {ifThenElse = False}
  | isPrefixOf "case" l && isSuffixOf "0" l
  = readCheckList ls $ checkl {caseIndent = False}
  | isPrefixOf "do" l && isSuffixOf "0" l
  = readCheckList ls $ checkl {doIndent = False}
  | isPrefixOf "let" l && isSuffixOf "0" l
  = readCheckList ls $ checkl {letIndent = False}
  | isPrefixOf "guard" l && isSuffixOf "0" l
  = readCheckList ls $ checkl {letIndent = False}
  | isPrefixOf "functionRhs" l && isSuffixOf "0" l
  = readCheckList ls $ checkl {rhsAlign = False}
  | isPrefixOf "equalstrue" l && isSuffixOf "0" l
  = readCheckList ls $ checkl {equalstrue = False}
  | isPrefixOf "signatures" l && isSuffixOf "0" l
  = readCheckList ls $ checkl {topLevelSig = False}
  | isPrefixOf "blankLines" l && isSuffixOf "0" l
  = readCheckList ls $ checkl {blankLines = False}
  | isPrefixOf "trailingSpaces" l && isSuffixOf "0" l
  = readCheckList ls $ checkl {trailingS = False}
  | isPrefixOf "moduleHeader" l && isSuffixOf "0" l
  = readCheckList ls $ checkl {moduleheader = False}
  | isPrefixOf "imports" l && isSuffixOf "0" l
  = readCheckList ls $ checkl {imports = False}
  | isPrefixOf "data" l && isSuffixOf "0" l
  = readCheckList ls $ checkl {dataIndent = False}
  | isPrefixOf "list" l && isSuffixOf "0" l
  = readCheckList ls $ checkl {listIndent = False}
  | isPrefixOf "thentrueelsefalse" l && isSuffixOf "0" l
  = readCheckList ls $ checkl {thenTrueElseFalse = False}
  | isPrefixOf "notEqual" l && isSuffixOf "0" l
  = readCheckList ls $ checkl {notEqual = False}
  | isPrefixOf "notOrd" l && isSuffixOf "0" l
  = readCheckList ls $ checkl {notOrd = False}
  | isPrefixOf "equalsEmptyList" l && isSuffixOf "0" l
  = readCheckList ls $ checkl {equalsEmptyList = False}
  | isPrefixOf "identFunc" l && isSuffixOf "0" l
  = readCheckList ls $ checkl {identFunc = False}
  | isPrefixOf "constFunc" l && isSuffixOf "0" l
  = readCheckList ls $ checkl {constFunc = False}
  | isPrefixOf "andOr" l && isSuffixOf "0" l
  = readCheckList ls $ checkl {andOr = False}
  | isPrefixOf "print" l && isSuffixOf "0" l
  = readCheckList ls $ checkl {printCheck = False}
  | isPrefixOf "deriving" l && isSuffixOf "0" l
  = readCheckList ls $ checkl {derivingIndent = False}
  | isPrefixOf "class" l && isSuffixOf "0" l
  = readCheckList ls $ checkl {classIndent = False}
  | isPrefixOf "instance" l && isSuffixOf "0" l
  = readCheckList ls $ checkl {instanceIndent = False}
  | otherwise
  = readCheckList ls checkl

--first line that starts with maxLineLength provides the length for lengthcheck
--if no option found, use default 80 charakters
--TODO: if no int at the end
readLength :: [String] -> Int -> Int
readLength [] len
  = len
readLength (l:ls) len
  | isPrefixOf "maxLineLength" l = readInt $ last $ words l
  | otherwise                    = readLength ls len

-- first line that starts with "hint" provides Bool if hints are on or off
readHint :: [String] -> Bool
readHint []
  = True
readHint (l:ls)
  | isPrefixOf "hints" l && isSuffixOf "0" l = False
  | otherwise                                = readHint ls

readCode :: [String] -> Bool
readCode []
  = True
readCode (l:ls)
  | isPrefixOf "code" l && isSuffixOf "0" l = False
  | otherwise                               = readCode ls

readOType :: [String] -> OutPut
readOType [] = TEXT
readOType (l:ls)
  | isPrefixOf "oType" l && isSuffixOf "JSON" l = JSON
  | otherwise                                   = readOType ls

readVerbosity :: [String] -> Int -> Int
readVerbosity [] n = if ((n < 4) && ( n > -1)) then n else 1
readVerbosity (l:ls) n
  | isPrefixOf "verbosity" l    = readVerbosity ls (readInt $ last $ words l)
  | otherwise                 = readVerbosity ls n
