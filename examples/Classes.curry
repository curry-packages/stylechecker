{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}

-- deriving ----------------------------------

-- erroneous alignment
data Tree1   = Leaf1
              | Tree1 Tree1
             | Node1
             deriving (Show)

-- erroneous deriving alignment
data Tree2   = Leaf2
             | Tree2 Tree2
             | Node2
              deriving (Show)

-- erroneous aligment symbols
data Tree3  = True1 | False1
            deriving (Eq
                       , Ord
                      , Show)

data Tree4  = True2 | False2
            deriving (Eq
                     , Ord
                     , Show
                     )

data Tree5  = Leaf5 | Node5
  deriving (Eq, Ord, Show)

-- data Tree1   = Tree1
--   { leaf1 :: Int
--   , tree1 :: Int
--   , node1 :: Int
--   } deriving (Show)

-- data Tree2   = Tree2
--   { l :: Int
--   , t :: Int
--   , n :: Int
--   } deriving (Show)

-- data Tree3  = Tree3
--   { true   :: Bool
--   , false  :: Bool
--   } deriving ( Eq
--              , Ord
--              , Show)

-- data Tree4  = Tree4
--   { true1  :: Bool
--   , false1 :: Bool
--   } deriving (Eq
--              , Ord
--              , Show
--              )

-- class declaration -----------------

class Test a where
  b :: a
  c :: a

-- erroneous identation
class Test1 a where
   d1 :: a
   e1 :: a

class Test2 a b | a -> b where
  d2 :: a -> b
  e2 :: b -> a

-- instance declaration -----------------------

-- error where
instance Test Tree2
  where
  b = Leaf2
  c = b

-- error indentation
instance Test Tree3 where
   b = False1
   c = b
