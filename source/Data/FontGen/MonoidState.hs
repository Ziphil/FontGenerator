{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}


module Data.FontGen.MonoidState
  ( MonoidState
  , runMonoidState
  , execMonoidState
  , execMonoidState'
  , add
  , getAddend
  , getVal
  )
where


-- モノイドの値を次々に演算していくという操作を行う状態を表す型です。
-- Function, Applicative, Monoid のインスタンスはタプルと全く同じです。
data MonoidState s a = s :&& a

fmapMonoid :: (a -> b) -> MonoidState s a -> MonoidState s b
fmapMonoid func (addend :&& val) = addend :&& func val

apMonoid :: Monoid s => MonoidState s (a -> b) -> MonoidState s a -> MonoidState s b
apMonoid (fstAddend :&& func) (sndAddend :&& val) = (fstAddend <> sndAddend) :&& func val

bindMonoid :: Monoid s => MonoidState s a -> (a -> MonoidState s b) -> MonoidState s b
bindMonoid (fstAddend :&& fstVal) action =
  case action fstVal of
    sndAddend :&& sndVal -> (fstAddend <> sndAddend) :&& sndVal

instance Functor (MonoidState s) where
  fmap = fmapMonoid

instance Monoid s => Applicative (MonoidState s) where
  pure = (mempty :&&)
  (<*>) = apMonoid

instance Monoid s => Monad (MonoidState s) where
  (>>=) = bindMonoid

runMonoidState :: Monoid s => MonoidState s a -> s -> (a, s)
runMonoidState (addend :&& val) init = (val, init <> addend)

execMonoidState :: Monoid s => MonoidState s a -> s -> s
execMonoidState state init = snd $ runMonoidState state init

execMonoidState' :: Monoid s => MonoidState s a -> s
execMonoidState' state = execMonoidState state mempty

-- 状態に保存されている値に与えられた値を演算します。
-- 一般的な状態モナドの modify に相当します。
add :: s -> MonoidState s ()
add addend = addend :&& ()

getAddend :: MonoidState s a -> s
getAddend (addend :&& _) = addend

getVal :: MonoidState s a -> a
getVal (_ :&& val) = val