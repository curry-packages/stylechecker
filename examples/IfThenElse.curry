
-- erroneous formatting: start then in own lines
ff = if
       True then 1
            else 2

-- erroneous formatting: start else in own line
ff = if True
        then 1 else 2

-- erroneous formatting: start then in own lines
ff = if
       True
       then
        1 else 2

ff = if True then
               1
             else
               2

-- erroneous formatting: fit in one line
ff = if True then 1 else
  2

-- erroneous formatting: start then and else in own lines
ff = if
       True then
              1 else 2


func1 x = if x then 1 else 0

func2 x = if x
            then 1
            else 0

func3 x = if x then 1
               else 0
func1 :: Bool -> Int
func4 :: Bool -> Int

func4 x =
  if x then 1
       else 0

-- erroneous else expr formatting
funcIf x =
  if x then 1 else
    0

-- erroneous indentation then-expression
funcIf x =
  if x then
    10000001010101
       else 42

funcIf x =
  if x then
         10000001010101
       else
         42

--erroneous formatting
funcIf x =
  if x then return 43 else do
    return 42

funcIf x =
  if x then return 43
       else do
         return 42

funcIf x =
  if
    x
    then 1
    else
      0

-- erroneous formatting else, then indentation
func5 x = if x
  then 1 else 0	

func6 x = if x
            then 1
            else 0

