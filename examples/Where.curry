--erroneous where body indentation
f x = f1 x
 where
   f1 y = y

--erroneous where, where body wrong indentation
f x = f1 x
  where
    f1 y = y

--erroneous where indentation
f x = f1 x
  where
   f1 y = y
   x = 1


f3 x =
  f1 x
 where
  f1 y = y
  x = 1

f x =
  f1 x
 where f1 y = y

f x = f1 x
 where
  f1 y = g y
   where
    g z = z

-- erroneous indentation
f2 x = f1 x
 where f1 y = g y
        where
         g z = z

-- erroneous indentationn
func6 x = f x
  where
    f x = if x then 1
               else 0
