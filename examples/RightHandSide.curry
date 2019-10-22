-- erroneous righthandside aligment
funca 1 _ _ = 1
funca 2 _ _ = 2
funca 3 x y | x == y    = 3
            | x < y     = 4
            | y > x     = 5
            | otherwise = 0
funca 4 x y | fu5 x     = 6
            | fu5 y     = 7
            | otherwise = 0
funca 5 x y = 0
-- erroneous symbol alignment
funcb 1 _ _    = 1
funcb 2 _ _ = 2
funcb 3 x y | x == y = 3
            | x < y  = 4
            | y > x  = 5
            | otherwise = 0
funcb 4 x y  | fu5 x = 6
            | fu5 y = 7
            | otherwise = 0
funcb 5 x y = 0

g0 x = if x
         then c
         else do
           a
           b

g1 x = case x of
  1 -> do
    a
    b
  _ -> return ()

g2 x = if x
         then 1
         else case x of
           True   -> 1
           False  -> 0

g3 x | x == 0 = do
       a
       b
       c
     | x == 1 = a

fu5 x = x

a = return ()
b = return ()
c = return ()
