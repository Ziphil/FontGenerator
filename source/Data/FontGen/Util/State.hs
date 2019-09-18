{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Data.FontGen.Util.State
  ( module Control.Monad.State
  , MonoidState
  , add
  , mapAddend
  , RunState (..)
  , execState
  , execState'
  )
where

import Control.Monad.State hiding (runState, execState)
import qualified Control.Monad.State as State


-- 通常の状態モナドと同じ働きをしますが、状態の変化がモノイドの要素の演算であった場合、特別にその値を保持しておきます。
-- この型の値を作るには、通常の状態モナドと同様に get, set, state などを使うことに加え、add を使うこともできます。
data MonoidState s a = Add a s | Run (s -> (a, s))

toState :: Monoid s => MonoidState s a -> State s a
toState (Add val addend) = State.state $ \current -> (val, current <> addend)
toState (Run runner) = State.state runner

fromState :: State s a -> MonoidState s a
fromState obj = Run $ State.runState obj

returnMonoid :: Monoid s => a -> MonoidState s a
returnMonoid val = Add val mempty

bindDefault :: Monoid s => MonoidState s a -> (a -> MonoidState s b) -> MonoidState s b
bindDefault obj action = fromState $ toState obj >>= toState . action

bindMonoid :: Monoid s => MonoidState s a -> (a -> MonoidState s b) -> MonoidState s b
bindMonoid obj@(Add val addend) action =
  case action val of
    Add finalVal nextAddend -> Add finalVal (addend <> nextAddend)
    Run _ -> bindDefault obj action
bindMonoid obj action = bindDefault obj action

instance Monoid s => Functor (MonoidState s) where
  fmap = liftM

instance Monoid s => Applicative (MonoidState s) where
  pure = return
  (<*>) = ap

instance Monoid s => Monad (MonoidState s) where
  return = returnMonoid
  (>>=) = bindMonoid

instance Monoid s => MonadState s (MonoidState s) where
  state = fromState . State.state

-- 状態に演算するモノイドの要素から、状態モナドの値を生成します。
add :: s -> MonoidState s ()
add addend = Add () addend

-- 状態に演算するモノイドの要素を変更します。
-- 状態の変化がモノイドの演算でなかった場合は、何もせずそのまま返します。
mapAddend :: (s -> s) -> MonoidState s a -> MonoidState s a
mapAddend func (Add val addend) = Add val (func addend)
mapAddend _ obj = obj

class RunState s m where
  runState :: m a -> s -> (a, s)

instance RunState s (State s) where
  runState = State.runState

instance Monoid s => RunState s (MonoidState s) where
  runState = State.runState . toState

execState :: RunState s m => m a -> s -> s
execState = (snd .) . runState

execState' :: (RunState s m, Monoid s) => m a -> s
execState' = flip execState mempty