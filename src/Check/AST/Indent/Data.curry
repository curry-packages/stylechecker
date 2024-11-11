module Check.AST.Indent.Data where

import Types

import Curry.SpanInfo
import Curry.Span
import Curry.Position
import Curry.Types
import Curry.Ident
import Text.Pretty

import Data.List     ( last )
import Control.Monad ( unless )

-- Applies actual check on Data constructs.
checkData :: Decl a -> Int -> CSM ()
checkData e _ =
  case e of
    (DataDecl sI _ _ constr _) -> checkData' sI constr
    _                          -> return ()

-- Applies check according to declaration type.
checkData' :: SpanInfo -> [ConstrDecl] -> CSM ()
checkData' sI (l:ls) = case l of
  (ConstrDecl _ _ _)  -> checkCData sI (l:ls)
  (RecordDecl _ _ _)  -> checkRecord sI (l:ls)
  _                   -> return ()
checkData' _ [] = return ()

-- Checks different formatting aspects.
checkCData :: SpanInfo -> [ConstrDecl] -> CSM ()
checkCData si (con:cons) = case si of 
  (SpanInfo (Span (Position ls cs) (Position le ce)) (_:(Span (Position l c) _):ks)) -> 
    unless (ls == (getLi (getSpanInfo (last (con:cons))))) --one line
      (if (ls == l) -- first line till equal sign
        then
          (if (not (null (cons))) -- one constructor
              then
                (if (spanAlign (take (length cons) ks)) -- alignment of bars
                  then
                    (if ((null ks) || ((getFirstBar ks) == (cs+2)) || (getFirstBar ks == c)) -- indentation
                        then
                          (unless (checkAlign getCol (getCol (getSpanInfo con)) cons) -- alignment of constructors
                            (report (Message (Span (Position ls cs) (Position le ce))
                                            (colorizeKey "data declaration constructors"<+> text "not aligned")
                                            ( text "align"
                                            <+> colorizeKey "constructors"
                                            )
                                    )
                            )
                          )
                        else
                          (report (Message (Span (Position ls cs) (Position le ce))
                                          (colorizeKey "data declaration" <+> text "body wrong indention")
                                          ( text "indent by 2"
                                          <+> colorizeKey "from declaration"
                                          <+> text "or write"
                                          <+> colorizeKey "="
                                          <+> text "in line with data"
                                          )
                                  )
                          )
                    )
                  else
                    (report (Message (Span (Position ls cs) (Position le ce))
                                      (colorizeKey "|" <+> text "of data declaration not aligned")
                                      ( text "align"
                                      <+> colorizeKey "|"
                                      )
                            )
                    )
                )
              else
                (report (Message (Span (Position ls cs) (Position le ce))
                                (colorizeKey "data declaration" <+> text "wrong formatting")
                                ( text "if only one constructor, write in one line"
                                )
                        )
                )
          )
        else
          (report (Message (Span (Position ls cs) (Position l c))
                                  (colorizeKey "data declaration" <+> text "wrong formatting")
                                  ( text "write"
                                  <+> colorizeKey "data"
                                  <+> text "till"
                                  <+> colorizeKey "="
                                  <+> text "in one line"
                                  )
                          )
                  )
      )
  _ -> return ()
checkCData _ [] = return ()

-- Returns column Position of first `|`.
getFirstBar :: [Span] -> Int
getFirstBar (k:_) = case k of 
  (Span (Position _ c) _) -> c
  _ -> -1 
getFirstBar [] = -1

-- Checks various formatting aspects of records.
checkRecord :: SpanInfo -> [ConstrDecl] -> CSM ()
checkRecord si (rd:_)
  = case (si, rd) of 
      (SpanInfo (Span (Position ls cs) (Position le ce)) _,
       (RecordDecl (SpanInfo _ symbs@((Span (Position _ c) _):_)) ident fs)) -> 
        if (ls == (getLi (getSpanInfo ident))) -- one line
          then
            (if (spanAlign symbs) -- alignment symbols { } ,
              then
                (if ((c == (cs + 2))||(c == (getCol (getSpanInfo ident)) + 2)) -- indentation of record body
                    then
                      (if (spanAlign (gatherSpanFromFieldDecls getNameSpan fs)) -- alignment names
                        then
                          (if (spanAlign (gatherSpanFromFieldDecls getSigSpan fs)) -- alignment ::
                              then
                                (unless (spanAlign (gatherSpanFromFieldDecls getTypeSpan fs)) -- alignment types
                                  (report (Message (Span (Position ls cs) (Position le ce))
                                                  (colorizeKey "Type" <+> text "of record fields not aligned")
                                                  ( text "align"
                                                  <+> colorizeKey "types"
                                                  )
                                          )
                                  )
                                )
                              else
                                (report (Message (Span (Position ls cs) (Position le ce))
                                                (colorizeKey "::" <+> text "of records fields not aligned")
                                                ( text "align"
                                                <+> colorizeKey "::"
                                                )
                                        )
                                )
                          )
                        else
                          (report (Message (Span (Position ls cs) (Position le ce))
                                          (colorizeKey "names :" <+> text "of record fields not aligned")
                                          ( text "align"
                                          <+> colorizeKey "names"
                                          )
                                  )
                          )
                      )
                    else
                      (report (Message (Span (Position ls cs) (Position le ce))
                                      (colorizeKey "record body" <+> text "wrong indentation")
                                      ( text "indent by"
                                      <+> colorizeKey "2"
                                      <+> text "from record declaration or name of record"
                                      )
                              )
                      )
                )
              else
                (report (Message (Span (Position ls cs) (Position le ce))
                                  (colorizeKey "record body" <+> text "not aligned")
                                  ( text "align"
                                  <+> colorizeKey "{"
                                  <+> text ","
                                  <+> colorizeKey "}"
                                  <+> text "and"
                                  <+> colorizeKey ","
                                  )
                          )
                  )
            )
          else
            (report (Message (Span (Position ls cs) (Position le ce)) 
                            (colorizeKey "record declaration" <+> text "wrong formatting")
                            ( text "write"
                            <+> colorizeKey "data"
                            <+> text "till name of record in one line"
                            )
                    )
            )
      _ -> error "checkRecord: SpanInfo or RecordDecl missing"
checkRecord _ [] = return ()

-- Picks out span of `::`.
getSigSpan :: FieldDecl -> Span
getSigSpan fd = case fd of
  (FieldDecl (SpanInfo _ (s:_)) _ _) -> s
  _ -> error "getSigSpan: NoSpan"

-- Picks out span of name.
getNameSpan :: FieldDecl -> Span
getNameSpan fd = case fd of 
  (FieldDecl _ ((Ident (SpanInfo s _ ) _ _):_) _) -> s
  _ -> error "getNameSpan: NoSpan"
-- Picks out span type.
getTypeSpan :: FieldDecl -> Span
getTypeSpan fd = case fd of 
  (FieldDecl _ _ (ConstructorType (SpanInfo s _) _)) -> s
  _ -> error "getTypeSpan: NoSpan"

-- Using a span picker, constructs a list of spans.
gatherSpanFromFieldDecls :: (FieldDecl -> Span) -> [FieldDecl] -> [Span]
gatherSpanFromFieldDecls _   []     = []
gatherSpanFromFieldDecls fun (f:fs) = [(fun f)]++(gatherSpanFromFieldDecls fun fs)
