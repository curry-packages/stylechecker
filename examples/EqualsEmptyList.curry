-- error
fz = [] == [2]

-- error
lz = []

-- error
gz = if ([1] /= []) then "ok" else "oh no"

-- error
hz = (==) [1,2,3] []
