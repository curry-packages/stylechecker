fu1 x | x == 0     = 0
      | x == 1     = 1
      | otherwise  = 2

fu1 x | x == 0     = 0
      | x == 1
                   = 1
      | otherwise  = 2

fu2 x
  | x == 0     = 0
  | x == 1     = 1
  | otherwise  = 2

fu3 x | x == 0
      = 0
      | x == 1
      = 1
      | otherwise
      = 2

--erroneous alignment
fu4 x | x == 0
        = 0
      | x == 1
          = 1
        | otherwise  = 2

--erroneous alignment equal signs, indentation
fu5 x
    | x == 0  = True
    | x == 1  = False
    | otherwise  = False
