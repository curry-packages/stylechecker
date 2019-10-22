-- superluos
g = not (1 < 2)

-- superfluos
g1 = if not (3 <= 4) then 0 else 1

g2 = 1 >= 4

--superfluos
g3 x y z = if x
             then case y of 
               1  -> z
               2  -> not (z)
               _  -> if (not (y > 5)) then x else not x
             else False

g4 = not ((<=) 2 4)
