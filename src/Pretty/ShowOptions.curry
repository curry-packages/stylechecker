module Pretty.ShowOptions where

import Pretty.ToString    (renderMessagesToString)
import Pretty.ToJson      (renderMessagesToJson)
import Types

-- returns a String containing the config settings
showOptions :: Config -> String
showOptions (Config checks output verbosity hint code mLength) =
  "--------------------------------"
  ++ "\nSETTINGS:"
  ++ "\nverbosity: " ++ (showVerbosity verbosity)
  ++ "\noutput format: " ++ (show output)
  ++ "\nshow hint: " ++ (showBool hint)
  ++ "\nshow code exerpt: " ++ (showBool code)
  ++ "\nmaximal linelength: " ++ (show mLength)
  ++ "\nlist of checks: " ++ (showCheckList checks)

-- details verbosity level
showVerbosity :: Int -> String
showVerbosity 0 = "quiet, only show check warnings"
showVerbosity 1 = "default, enable showing hints and code part, segment output of each file"
showVerbosity 2 = "verbose, INFO and WARNING messages concerning progress of the tool are shown"
showVerbosity 3 = "debug, show options"

-- return on and off instead of true and false
showBool :: Bool -> String
showBool True = "on"
showBool False = "off"

-- renders checklist
showCheckList :: CheckList -> String
showCheckList (CheckList lineLength
                         tab
                         trailingS
                         whiteSpace
                         ifThenElse
                         doIndent
                         letIndent
                         caseIndent
                         guardIndent
                         rhsAlign
                         whereIndent
                         equalstrue
                         topLevelSig
                         blankLines
                         moduleheader
                         imports
                         dataIndent
                         listIndent
                         thenTrueElseFalse
                         notEqual
                         notOrd
                         equalsEmptyList
                         identFunc
                         constFunc
                         andOr
                         printCheck
                         derivingIndent
                         classIndent
                         instanceIndent
                         ) =
  "\n  general"
  ++ "\n    length exceeding lines : " ++ (showBool lineLength)
  ++ "\n    tab characters : " ++ (showBool tab)
  ++ "\n    trailing spaces : " ++ (showBool trailingS)
  ++ "\n    other whitespaces (except linebreaks) : " ++ (showBool whiteSpace)
  ++ "\n    signatures (positioning): " ++ (showBool topLevelSig)
  ++ "\n    blank lines: " ++ (showBool blankLines)
  ++ "\n  formatting"
  ++ "\n    if-then-else : " ++ (showBool ifThenElse)
  ++ "\n    do: " ++ (showBool doIndent)
  ++ "\n    let: " ++ (showBool letIndent)
  ++ "\n    case: " ++ (showBool caseIndent)
  ++ "\n    guards: " ++ (showBool guardIndent)
  ++ "\n    guards and equations in a function: " ++ (showBool rhsAlign)
  ++ "\n    where: " ++ (showBool whereIndent)
  ++ "\n    module header : " ++ (showBool moduleheader)
  ++ "\n    import declarations : " ++ (showBool imports)
  ++ "\n    data (and record) declarations: " ++ (showBool dataIndent)
  ++ "\n    list and tuple declarations: " ++ (showBool listIndent)
  ++ "\n    fold (||), (&&) : " ++ (showBool andOr)
  ++ "\n    deriving : " ++ (showBool derivingIndent)
  ++ "\n    class declarations : " ++ (showBool classIndent)
  ++ "\n    instance declarations : " ++ (showBool instanceIndent)
  ++ "\n  code linting (patterns)"
  ++ "\n    x == True: " ++ (showBool equalstrue)
  ++ "\n    if x then True (False) else False (True): " ++ (showBool thenTrueElseFalse)
  ++ "\n    not (a == b): " ++ (showBool notEqual)
  ++ "\n    not (a orderingOperation b): " ++ (showBool notOrd)
  ++ "\n    a == []: " ++ (showBool equalsEmptyList)
  ++ "\n    \\x -> x: " ++ (showBool identFunc)
  ++ "\n    \\x y -> x: " ++ (showBool constFunc)
  ++ "\n    fold (||), (&&) : " ++ (showBool andOr)
  ++ "\n    putStrLn (show a) : " ++ (showBool printCheck)
