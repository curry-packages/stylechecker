-- error
f l = foldr (||) l
f l = foldl (&&) l
