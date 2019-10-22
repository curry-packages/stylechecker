module Check.AST.Indent.Data where

import Types

import Curry.SpanInfo
import Curry.Span
import Curry.Position
import Curry.Types
import Curry.Ident
import Text.Pretty

import List  (last)

-- applies actual check on Data constructs
checkData :: Decl a -> Int -> CSM ()
checkData e _ =
  case e of
    (DataDecl sI _ _ constr@(l:ls) _) -> checkData' sI constr
    _                                 -> return ()

-- apply check according to declaration type
checkData' :: SpanInfo -> [ConstrDecl] -> CSM ()
checkData' sI (l:ls) = case l of
  (ConstrDecl _ _ _ _ _)  -> checkCData sI (l:ls)
  (RecordDecl _ _ _ _ _)  -> checkRecord sI (l:ls)
  _                       -> return ()

-- check different formatting ascpects
checkCData :: SpanInfo -> [ConstrDecl] -> CSM ()
checkCData (SpanInfo (Span (Position ls cs) (Position le ce)) sp@(k:(Span (Position l c) _):ks)) (con:cons) =
  unlessM (ls == (getLi (getSpanInfo (last (con:cons))))) --one line
    (if (ls == l) -- first line till equal sign
       then
         (if (not (null (cons))) -- one constructor
            then
              (if (spanAlign (take (length cons) ks)) -- alignment of bars
                 then
                   (if ((null ks) || ((getFirstBar ks) == (cs+2)) || (getFirstBar ks == c)) -- indentation
                      then
                        (unlessM (checkAlign getCol (getCol (getSpanInfo con)) cons) -- alignment of constructors
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

-- returns column Position of first |
getFirstBar :: [Span] -> Int
getFirstBar ((Span (Position _ c) _):ks) = c
getFirstBar [] = -1

-- checks various formatting aspects of records
checkRecord :: SpanInfo -> [ConstrDecl] -> CSM ()
checkRecord (SpanInfo (Span (Position ls cs) (Position le ce)) _)
            ((RecordDecl (SpanInfo _ symbs@((Span (Position _ c) _):_)) _ _ ident fs):_)
            = if (ls == (getLi (getSpanInfo ident))) -- one line
                then
                  (if (spanAlign symbs) -- alignment symbols { } ,
                     then
                       (if ((c == (cs + 2))||(c == (getCol (getSpanInfo ident)) + 2)) -- indentation of record body
                          then
                            (if (spanAlign (gatherSpanFromFieldDecls getNameSpan fs)) -- alignment names
                               then
                                 (if (spanAlign (gatherSpanFromFieldDecls getSigSpan fs)) -- alignment ::
                                    then
                                      (unlessM (spanAlign (gatherSpanFromFieldDecls getTypeSpan fs)) -- alignment types
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

-- pick out span of ::
getSigSpan :: FieldDecl -> Span
getSigSpan (FieldDecl (SpanInfo _ (s:_)) _ _) = s

-- pick out span of name
getNameSpan :: FieldDecl -> Span
getNameSpan (FieldDecl _ ((Ident (SpanInfo s _ ) _ _):_) _) = s

-- pick out span type
getTypeSpan :: FieldDecl -> Span
getTypeSpan (FieldDecl _ _ (ConstructorType (SpanInfo s _) _)) = s

-- using a span picker, construct a list of spans
gatherSpanFromFieldDecls :: (FieldDecl -> Span) -> [FieldDecl] -> [Span]
gatherSpanFromFieldDecls _   []     = []
gatherSpanFromFieldDecls fun (f:fs) = [(fun f)]++(gatherSpanFromFieldDecls fun fs)

