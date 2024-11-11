
-- Error: blank line missing between equation of `fun1`
-- and type signature of `fun2`
fun1 :: Maybe Int -> Int
fun1 (Just x) = x
fun1 Nothing  = 42
fun2 :: Int -> Int
fun2 = (+ 1)

fun3 :: Either (Maybe Int) () -> Int
fun3 (Left (Just x)) = x
fun3 (Left Nothing)  = 42

fun4 :: a -> Int
fun4 = const 42

fun3 (Right ())      = 0