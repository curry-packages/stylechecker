module State
  ( State (runState), state, get, put, modify, evalState, execState
  ) where

newtype State s a = State { runState :: s -> (a, s) }

instance Monad (State s) where
  return x = state (\s -> (x, s))
  m >>= f = state (\s -> let (x, s') = runState m s
                         in runState (f x) s')

state :: (s -> (a, s)) -> State s a
state = State

get :: State s s
get = state (\s -> (s, s))

put :: s -> State s ()
put s = state (\_ -> ((), s))

modify :: (s -> s) -> State s ()
modify f = state (\s -> ((), f s))

evalState :: State s a -> s -> a
evalState m s = fst (runState m s)

execState :: State s a -> s -> s
execState m s = snd (runState m s)
