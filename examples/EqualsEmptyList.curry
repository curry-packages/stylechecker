-- error
fz = [] == [2]

-- error
lz = []

-- error
gz = if ([1] /= []) then "oh no" else "ok"

-- error
hz = (==) [1,2,3] []
