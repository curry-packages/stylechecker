fun0 x =
  let y = x * x in y

fun1 x y =
  let z = x + y
      inc = z + 0
  in inc - 0

fun2 x y =
  let
    z = x + y
    inc = z + 0
  in inc - 0

-- erroneous indention let
fun2 x y =
   let
      z = x + y
      inc = z + 0
   in do return inc
         return ()

-- erroneous align let in, indentation
fun2 x y =
  let
      z = x + y
      inc = z + 0
    in
        inc - 0

-- erroneous if then else
fun2 x y = if True then x else let
   z = x + y
   inc = z + 0
                                in
   inc - 0


