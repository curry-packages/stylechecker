module Types where

import Curry.Types
import Curry.Position
import Curry.SpanInfo
import Curry.Span
import Curry.Ident
import Text.Pretty

import Control.Monad.Trans.State ( State, get, modify )

import Prelude hiding ( ifThenElse )

-- A line of String with an index.
type SrcLine = (Int, String)

-- Flag type for commandline operations.
data Flag
  = Ignore String
  | Add String
  | OType String
  | Verbosity Int
  | Help
  deriving (Eq, Show)

type ProgramName = String

type Arguments = (ProgramName, [Flag], [String])

-- Part of config, records which checks are on/off.
data CheckList = CheckList
                 { lineLength        :: Bool
                 , tab               :: Bool
                 , trailingS         :: Bool
                 , whiteSpace        :: Bool
                 , ifThenElse        :: Bool
                 , doIndent          :: Bool
                 , letIndent         :: Bool
                 , caseIndent        :: Bool
                 , guardIndent       :: Bool
                 , rhsAlign          :: Bool
                 , whereIndent       :: Bool
                 , equalstrue        :: Bool
                 , topLevelSig       :: Bool
                 , blankLines        :: Bool
                 , moduleheader      :: Bool
                 , imports           :: Bool
                 , dataIndent        :: Bool
                 , listIndent        :: Bool
                 , thenTrueElseFalse :: Bool
                 , notEqual          :: Bool
                 , notOrd            :: Bool
                 , equalsEmptyList   :: Bool
                 , identFunc         :: Bool
                 , constFunc         :: Bool
                 , andOr             :: Bool
                 , printCheck        :: Bool
                 , derivingIndent    :: Bool
                 , classIndent       :: Bool
                 , instanceIndent    :: Bool
                 } deriving (Show)

-- Checks if any of the AST checks are on in config.
anyAST :: Config -> Bool
anyAST con =
  let c = (checks con)
  in or
       [ ifThenElse        c
       , doIndent          c
       , letIndent         c
       , caseIndent        c
       , guardIndent       c
       , rhsAlign          c
       , whereIndent       c
       , equalstrue        c
       , topLevelSig       c
       , blankLines        c
       , moduleheader      c
       , imports           c
       , dataIndent        c
       , listIndent        c
       , thenTrueElseFalse c
       , notEqual          c
       , notOrd            c
       , equalsEmptyList   c
       , identFunc         c
       , constFunc         c
       , andOr             c
       , printCheck        c
       , derivingIndent    c
       , classIndent       c
       , instanceIndent    c
       ]

-- Checks if any of the Src checks are on in config.
anySrc :: Config -> Bool
anySrc con =
  let c = (checks con)
  in or
       [ lineLength  c
       , tab         c
       , trailingS   c
       , whiteSpace  c
       ]

-- Record that is a compilation of all checks (of different types) on the AST.
data Checks a = Checks
                { modu          :: (Module a -> Int -> CSM ())
                , decl          :: (Decl a -> Int -> CSM ())
                , eq            :: (Equation a -> Int -> CSM ())
                , rhs           :: (Rhs a -> Int -> CSM ())
                , cExpr         :: (CondExpr a -> Int -> CSM ())
                , expr          :: (Expression a -> Int -> CSM ())
                , stat          :: (Statement a -> Int -> CSM ())
                , alt           :: (Alt a -> Int -> CSM ())
                }

-- Output types
data OutPut = JSON | TEXT
  deriving (Show)

-- Config record has a checklist and a maxLineLength.
data Config = Config
              { checks         :: CheckList
              , oType          :: OutPut
              , verbosity      :: Int
              , hints          :: Bool
              , code           :: Bool
              , maxLineLength  :: Int
              }

-- Message with relevant span and a warning-doc and hint-doc.
data Message = Message Span Doc Doc

-- Compare by line and if same, column.
instance Ord Message where
  (Message sp1 _ _) <= (Message sp2 _ _) = case (sp1, sp2) of 
    (Span (Position l1 c1) _,
     Span (Position l2 c2) _) -> l1 < l2 || (l1 == l2) && (c1 <= c2)
    _ -> error "Ord Message: NoSpan"
       
-- Only equal if line and column are the same.
instance Eq Message where
  (Message sp1 _ _) == (Message sp2 _ _) = case (sp1, sp2) of 
    (Span (Position l1 c1) _, (Span (Position l2 c2) _)) 
      -> l1 == l2 && c1 == c2
    _ -> error "Eq Message: NoSpan"

-- State that has a filename, a config and a list of messages.
data CheckState = CheckState { fileName :: String
                             , config :: Config
                             , messages :: [Message]
                             }

-- Checkstate monad.
type CSM = State CheckState

-- Adds messages to the message list in CSM.
report :: Message -> CSM ()
report m = modify $ \cs -> cs { messages = m : messages cs }

-- Returns config from CSM.
getConfig :: CSM Config
getConfig = do s <- get
               return $ config s

-- Returns checklist (from config) from CSM.
getCheckList :: CSM CheckList
getCheckList = do c <- getConfig
                  return $ checks c

-- Class that is traversed (checkChildren) and checked in checkAST.
class Checkable c where
  -- Applies right checks on construct and traverse the children by passing
  -- indentation edge (Int) and checks on to checkChildren.
  checkNode :: Checks a -> Int -> c a -> CSM ()
  -- Actually uses patternmatching to get children,
  -- recalculates new edge for each and passes these and the checks on to them.
  checkChildren :: Int -> Checks a -> c a -> CSM ()

-- Returns startcolumn of Spaninfo.
getCol :: SpanInfo -> Int
getCol sp = case sp of 
  (SpanInfo (Span (Position _ cp) _) _) -> cp
  _ -> error "getCol: NoSpan"

-- Returns startline of Spaninfo.
getLi :: SpanInfo -> Int
getLi sp = case sp of
  (SpanInfo (Span (Position lp _) _) _) -> lp
  _ -> error "getLi: NoSpan"

-- Returns endline of Spaninfo.
getEndLi :: SpanInfo -> Int
getEndLi sp = case sp of
  (SpanInfo (Span _ (Position lp _)) _) -> lp
  _ -> error "getEndLi: NoSpan"

-- Returns first argument (a Span) from Spaninfo.
getSpan :: HasSpanInfo a => a -> Span
getSpan x = let (SpanInfo s _) = getSpanInfo x
            in s

-- Returns startline of Span.
getSpanLi :: Span -> Int
getSpanLi sp = case sp of 
  (Span (Position l _) _) -> l
  _ -> error "getSpanLi: NoSpan"

-- Returns endline of Span.
getSpanCol :: Span -> Int
getSpanCol sp = case sp of
  (Span (Position _ c) _) -> c
  _ -> error "getSpanCol: NoSpan"

-- Returns `true` if list of `HasSpanInfo` are aligned with an int
-- at a given position (by passing a
-- function that selects the relevant int from the Spaninfos).
checkAlign :: HasSpanInfo a => (SpanInfo -> Int) -> Int -> [a] -> Bool
checkAlign f c (a:as@(_:_)) =
  (c == (f (getSpanInfo a))) && checkAlign f (f (getSpanInfo a)) as
checkAlign f c [a]          = (c == (f (getSpanInfo a)))
checkAlign _ _ []           = True

spanAlign :: [Span] -> Bool
spanAlign (_:[])   = True
spanAlign []       = True
spanAlign (a:b:bs) = case (a,b) of 
  ((Span (Position _ c0) _), (Span (Position _ c2) _)) -> (c0 == c2) && spanAlign (b:bs)
  _ -> False

-- Creates a colorized Doctype from a String and colorizing function.
colorizeText :: (Doc -> Doc) -> String -> Doc
colorizeText c s = c $ text s

-- Colorizes a stromg with colorizeKey.
colorizeKey :: String -> Doc
colorizeKey = colorizeText (\ s -> text "`" <> s <> text "`")

-- color/styling for keywords in messages
-- keyColor :: (Doc -> Doc)
-- keyColor = cyan

-- Returns relevant parts for pattern checks in infix compare checks (spaninfo, lefthandside,
-- operation, righthandside).
checkInfixCompare :: Expression a -> (SpanInfo, Expression a, String, Expression a)
checkInfixCompare e = case e of
  (InfixApply
    sI
    exp1
    (InfixOp _
      (QualIdent _ _
        (Ident _ op _)))
    exp2)
    -> (sI, exp1, op, exp2)
  _ -> error "checkInfixCompare: not an infix expression"

-- Returns relevant parts for pattern checks in compare checks (spaninfo,
-- operation, first expression, second expression).
checkApplyCompare :: Expression a -> (SpanInfo, String, Expression a, Expression a)
checkApplyCompare e = case e of
  (Apply sI
    (Apply _
      (Variable _ _
        (QualIdent _ _
          (Ident _ op _)
        )
      )
      exp1)
    exp2)
    -> (sI, op, exp1, exp2)
  _ -> error "checkApplyCompare: not an apply expression"