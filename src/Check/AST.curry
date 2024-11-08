module Check.AST where

import Control.Applicative ( when )
import Curry.Types
import Curry.SpanInfo
import Curry.Span
import Curry.Position

import Types
import Control.Monad.Trans.State
import Check.AST.Indent.IfThenElse         ( checkIfThenElse )
import Check.AST.Indent.Do                 ( checkDo )
import Check.AST.Indent.Let                ( checkLet )
import Check.AST.Indent.Case               ( checkCase )
import Check.AST.Indent.Guard              ( checkGuard )
import Check.AST.Indent.FuncRhs            ( checkRhs )
import Check.AST.Indent.Where              ( checkWhere )
import Check.AST.Indent.Header             ( checkModuleHeader )
import Check.AST.Indent.Imports            ( checkImports )
import Check.AST.Indent.Data               ( checkData )
import Check.AST.Indent.ListTuple          ( checkListTuple )
import Check.AST.Indent.Deriving           ( checkDeriving )
import Check.AST.Indent.Class              ( checkClass )
import Check.AST.Indent.Instance           ( checkInstance )
import Check.AST.Pattern.EqualsTrue        ( checkEqualsTrue )
import Check.AST.Pattern.ThenTrueElseFalse ( checkThenTrueElseFalse )
import Check.AST.Pattern.NotEqual          ( checkNotEqual )
import Check.AST.Pattern.NotOrd            ( checkNotOrd )
import Check.AST.Pattern.EqualsEmptyList   ( checkEqualsEmptyList )
import Check.AST.Pattern.IdentFunc         ( checkIdentFunc )
import Check.AST.Pattern.ConstFunc         ( checkConstFunc )
import Check.AST.Pattern.AndOr             ( checkAndOr )
import Check.AST.Pattern.Print             ( checkPrint )
import Check.AST.TopLevel.Signatures       ( checkTopLevelSig )
import Check.AST.TopLevel.BlankLines       ( checkBlankLines )

import Prelude hiding ( ifThenElse )

-- gets a selector on the checklist in the config and a check as well as
-- the two parameters,
-- if the selected key in checklist (check) is True, therefore "on",
-- check is appled on arguments
checkConf :: (CheckList -> Bool) -> (a -> Int -> CSM ()) -> a -> Int -> CSM ()
checkConf sel fun e i = do
                          c <- getCheckList
                          when (sel c) (fun e i)

default_ :: a -> b -> CSM ()
default_ = \_ _ -> return ()

-- all Checks in form of record, that are to applied on their types,
-- if in config a certain key is True, check will be applied
checks :: Checks a
checks = Checks (\e i -> do checkConf topLevelSig checkTopLevelSig e i
                            checkConf blankLines checkBlankLines e i
                            checkConf moduleheader checkModuleHeader e i
                            checkConf imports checkImports e i)
                (\e i -> do checkConf rhsAlign checkRhs e i
                            checkConf whereIndent checkWhere e i
                            checkConf dataIndent checkData e i
                            checkConf derivingIndent checkDeriving e i
                            checkConf classIndent checkClass e i
                            checkConf instanceIndent checkInstance e i)
                (\e i -> do checkConf guardIndent checkGuard e i)
                (\e i -> do checkConf listIndent checkListTuple e i)
                default_
                (\e i -> do checkConf ifThenElse checkIfThenElse e i
                            checkConf doIndent checkDo e i
                            checkConf letIndent checkLet e i
                            checkConf caseIndent checkCase e i
                            checkConf equalstrue checkEqualsTrue e i
                            checkConf thenTrueElseFalse checkThenTrueElseFalse e i
                            checkConf notEqual checkNotEqual e i
                            checkConf notOrd checkNotOrd e i
                            checkConf equalsEmptyList checkEqualsEmptyList e i
                            checkConf identFunc checkIdentFunc e i
                            checkConf constFunc checkConstFunc e i
                            checkConf andOr checkAndOr e i
                            checkConf printCheck checkPrint e i)
                default_
                default_

-- takes a function, which selects a certain line Position of a key of the
-- construct in the given spanInfo (which is the parent)
-- and compares to line of current child
-- if they are in the same line, return the indentation edge for the parent
-- (since the child adopts it)
-- else, the new edge is the child itself
--
-- the indentation edge is used in checks to get the right indentation for
-- childs of current construct (in this case the grandchildren!)
newIndent :: HasSpanInfo a => (SpanInfo -> Int) -> SpanInfo -> a -> Int -> Int
newIndent f sI a i = if (f sI) == (getLi (getSpanInfo a)) then i else (getCol (getSpanInfo a))

-- start traversing by putting complete AST, checks and start of line "1" into first checkNode
checkAST :: Module a -> CSM ()
checkAST rootE = checkNode checks 1
                 rootE

instance Checkable Module where
  checkChildren i c (Module _ _ _ _ _ _ decls) = mapM_ (\d -> checkNode c i d) decls

  checkNode c i m = do (modu c) m i
                       checkChildren i c m

instance Checkable Decl where
  checkChildren _ _ (InfixDecl {}                 ) = return ()
  checkChildren _ _ (DataDecl {}                  ) = return ()
  checkChildren _ _ (ExternalDataDecl {}          ) = return ()
  checkChildren _ _ (NewtypeDecl {}               ) = return ()
  checkChildren _ _ (TypeDecl {}                  ) = return ()
  checkChildren _ _ (TypeSig {}                   ) = return ()
  checkChildren _ c (FunctionDecl sI _ _ eqs      ) = mapM_ (\d -> checkNode c (getCol sI) d) eqs
  checkChildren _ _ (ExternalDecl {}              ) = return ()
  checkChildren i c (PatternDecl sI _ rhs         ) = checkNode c (newIndent getLi sI rhs i) rhs
  checkChildren _ _ (FreeDecl {}                  ) = return ()
  checkChildren _ _ (DefaultDecl {}               ) = return ()
  checkChildren i c (ClassDecl sI _ _ _ _ _ decls ) = mapM_ (\d -> checkNode c (newIndent getLi sI d i) d) decls
  checkChildren i c (InstanceDecl sI _ _ _ _ decls) = mapM_ (\d -> checkNode c (newIndent getLi sI d i) d) decls

  checkNode c i d = do (decl c) d i
                       checkChildren i c d

instance Checkable Equation where
  checkChildren i c (Equation _ _ lhs rhs) = checkNode c (newIndent getEndLi (getSpanInfo lhs) rhs i) rhs

  checkNode c i e = do (eq c) e i
                       checkChildren i c e

instance Checkable Rhs where
  checkChildren i c (SimpleRhs sI _ exp decls) = do 
    checkNode c (newIndent getLi sI exp i) exp
    mapM_ (\d -> checkNode c (newIndent getLi sI d i) d) decls
  checkChildren i c (GuardedRhs sI _ condExps decls) = do 
    mapM_ (checkNode c i) condExps
    mapM_ (\d -> checkNode c (newIndent getLi sI d i) d) decls

  checkNode c i r = do (rhs c) r i
                       checkChildren i c r

instance Checkable CondExpr where
  checkChildren _ c (CondExpr sI exp1 exp2) = do 
    checkNode c (getCol sI) exp1
    checkNode c (getCol sI) exp2

  checkNode c i cE = do 
    (cExpr c) cE i
    checkChildren i c cE

instance Checkable Expression where
  checkChildren _ _ (Literal _ _ _                     ) = return ()
  checkChildren _ _ (Variable _ _ _                    ) = return ()
  checkChildren _ _ (Constructor _ _ _                 ) = return ()
  checkChildren i c (Paren sI exp                      ) = checkNode c (newIndent getLi sI exp i) exp
  checkChildren i c (Typed sI exp _                    ) = checkNode c (newIndent getLi sI exp i) exp
  checkChildren _ _ (Record _ _ _ _                    ) = return ()
  checkChildren i c (RecordUpdate sI exp _             ) = checkNode c (newIndent getLi sI exp i) exp
  checkChildren i c (Tuple sI exps                     ) = mapM_ (\d -> checkNode c (newIndent getLi sI d i) d) exps
  checkChildren i c (List  sI _ exps                   ) = mapM_ (\d -> checkNode c (newIndent getLi sI d i) d) exps
  checkChildren i c (ListCompr sI exp ss               ) = do checkNode c (newIndent getLi sI exp i) exp
                                                              mapM_ (\d -> checkNode c (newIndent getLi sI d i) d) ss
  checkChildren i c (EnumFrom sI exp                   ) = do checkNode c (newIndent getLi sI exp i) exp
  checkChildren i c (EnumFromThen sI exp1 exp2         ) = do checkNode c (newIndent getLi sI exp1 i) exp1
                                                              checkNode c (newIndent getLi sI exp2 i) exp2
  checkChildren i c (EnumFromTo sI exp1 exp2           ) = do checkNode c (newIndent getLi sI exp1 i) exp1
                                                              checkNode c (newIndent getLi sI exp2 i) exp2
  checkChildren i c (EnumFromThenTo sI exp1 exp2 exp3  ) = do checkNode c (newIndent getLi sI exp1 i) exp1
                                                              checkNode c (newIndent getLi sI exp2 i) exp2
                                                              checkNode c (newIndent getLi sI exp3 i) exp3
  checkChildren i c (UnaryMinus sI exp                 ) = checkNode c (newIndent getLi sI exp i) exp
  checkChildren i c (Apply sI exp1 exp2                ) = do checkNode c (newIndent getLi sI exp1 i) exp1
                                                              checkNode c (newIndent getLi sI exp2 i) exp2
  checkChildren i c (InfixApply sI exp1 _ exp2         ) = do checkNode c (newIndent getLi sI exp1 i) exp1
                                                              checkNode c (newIndent getLi sI exp2 i) exp2
  checkChildren i c (LeftSection sI exp _              ) = checkNode c (newIndent getLi sI exp i) exp
  checkChildren i c (RightSection sI _ exp             ) = checkNode c (newIndent getLi sI exp i) exp
  checkChildren i c (Lambda sI _ exp                   ) = checkNode c (newIndent getLi sI exp i) exp
  checkChildren i c (Let sI _ decls exp                ) =
    do mapM_ (\d -> checkNode c (newIndent getLi sI d i) d) decls
       if ((getLi sI) == (getInLi sI)) -- if let and in in one line
         then checkNode c (newIndent getInLi sI exp i) exp -- use In Edge
         else checkNode c (newIndent getInLi sI exp (getInCol sI)) exp -- else use start of in
  checkChildren i c (Do sI _ ss exp                    ) = do mapM_ (\d -> checkNode c (getCol (getSpanInfo d)) d) ss
                                                              checkNode c (newIndent getLi sI exp i) exp
  checkChildren i c (IfThenElse sI exp1 exp2 exp3      ) =
    do checkNode c (newIndent getLi sI exp1 i) exp1 --If expression as usual
       if ((getLi sI) == (getThenLi sI)) -- if then is on the same line as if, as usual
         then checkNode c (newIndent getLi sI exp2 i) exp2
         else checkNode c (newIndent getThenLi sI exp2 (getThenCol sI)) exp2 --else, then indentation
       if ((getLi sI) == (getElseLi sI)) --if else on same line as if, as usual
         then checkNode c (newIndent getLi sI exp3 i) exp3
         else if ((getThenLi sI) == (getElseLi sI)) --if else on same line as then, then indentation
                then checkNode c (newIndent getThenLi sI exp3 (getThenCol sI)) exp3
                else checkNode c (newIndent getElseLi sI exp3 (getElseCol sI)) exp3 -- else else identation
  checkChildren i c (Case sI _ _ exp alts                ) = do checkNode c (newIndent getLi sI exp i) exp
                                                                mapM_ (\d -> checkNode c (newIndent getLi sI d i) d) alts

  checkNode c i e = do (expr c) e i
                       checkChildren i c e

-- Returns line position of `Then` from SpanInfo.
getThenLi :: SpanInfo -> Int
getThenLi si = case si of
  (SpanInfo _ [_,(Span (Position l _)_),_]) -> l
  _ -> error "getThenLi: NoSpanInfo"

-- Returns column position of `Then` from Spaninfo.
getThenCol :: SpanInfo -> Int
getThenCol si = case si of 
  (SpanInfo _ [_,(Span (Position _ c)_),_]) -> c
  _ -> error "getThenCol: NoSpanInfo"

-- Returns line position of `Else` from Spaninfo.
getElseLi :: SpanInfo -> Int
getElseLi si = case si of 
  (SpanInfo _ [_,_,(Span (Position l _)_)]) -> l
  _ -> error "getElseLi: NoSpanInfo"

-- Returns column position of `Else` from Spaninfo.
getElseCol :: SpanInfo -> Int
getElseCol si = case si of
  (SpanInfo _ [_,_,(Span (Position _ c)_)]) -> c
  _ -> error "getElseCol: NoSpanInfo"

-- Returns line pos of `In` keyword.
getInLi :: SpanInfo -> Int
getInLi si = case si of
  (SpanInfo _ [_,(Span (Position l _) _)]) -> l
  _ -> error "getInLi: NoSpanInfo"

-- Returns column pos of `In` keyword.
getInCol :: SpanInfo -> Int
getInCol si = case si of
  (SpanInfo _ [_,(Span (Position _ c) _)]) -> c
  _ -> error "getInCol: NoSpanInfo"

instance Checkable Statement where
  checkChildren i c (StmtExpr sI exp)     = checkNode c (newIndent getLi sI exp i) exp
  checkChildren i c (StmtDecl sI _ decls) = mapM_ (\d -> checkNode c (newIndent getLi sI d i) d) decls
  checkChildren i c (StmtBind sI _ exp  ) = checkNode c (newIndent getLi sI exp i) exp

  checkNode c i s = do (stat c) s i
                       checkChildren i c s

instance Checkable Alt where
  checkChildren i c (Alt sI _ rhs) = checkNode c (newIndent getLi sI rhs i) rhs

  checkNode c i a = do (alt c) a i
                       checkChildren i c a
