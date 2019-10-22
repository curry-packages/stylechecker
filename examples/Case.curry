-- casetests...............................................
--erroneous alignment
function1 x = case x of
  1   -> 1
  2 -> 2
  3 -> 3
  0 -> 0
--erroneous indentation
function2 x = case x of
    2 -> 2
    3 -> 3

function3 x = case x of
                1 -> 1
                0 -> 0

function4 x y = if x
                  then
                    case y of
                      1 -> 0
                      0 -> 1
                  else 2

function5 x y = if x
                  then case y of
                    1 -> 0
                    0 -> 1
                  else 2

function6 x y = if x then case y of
  1 -> 0
  0 -> 1
                     else 2

function7 x y = if x then case y of
                            1 -> 0
                            0 -> 1
                     else 2

function8 x y = do case y of
                     1 -> return 0
                     0 -> return 1

function9 x y = let x = case y of
                          1 -> return 0
                          0 -> return 1
                in x

function10 x = do case x of
                    0 -> return 0
                  case x of
                    1 -> return 1

function11 x = do
                 case x of
                   0 -> return 0
                 case x of
                   1 -> return 1

