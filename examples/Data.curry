-- erroneous aligment
data Tree1 a =
     Leaf1
   | Branch1 a
     | End1

data T = A | B

-- write in one line
data S =
  Test

data Bit = Zero | One

data Person1 = Person1
             { firstname :: String
             , lastName  :: String
             , age       :: Int
             }

-- erroneous aligment symbols
data Person2 = Person2
             { firstname1 :: String
             , lastName1  :: String
             , age1       :: Int
              }

-- erroneous aligment ::
data Person3 = Person3
             { firstname2 :: String
             , lastName2   :: String
             , age2       :: Int
             }
