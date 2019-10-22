--do tests ---------------------------------------

f x = if x then do
  return ()
  return ()
           else return ()

f x = if x
        then do
          a
          b
        else do if
                  x
                  then
                    c
                  else
                    a
-- erroneous aligment c, a
f x = if x
        then do
          a
          b
        else do if
                  x
                  then
                     c
                  else
                   a

f1 e = do case e of
            1 -> return ()
          b

f1 e = do case e of
            1 -> return ()
          b

-- erroneous indentation
f2 = do
    a
    b
    c

a = return ()

b = return ()

c = return ()
