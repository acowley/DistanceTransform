{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- |A newtype wrapper that provides a 'Num' instance for values in the
-- 'ST' monad. This makes arithmetic on mutable arrays somewhat more
-- concise.
module STNum where
import Control.Applicative
import Control.Monad.ST

newtype STNum s a = STNum { stnum :: ST s a } deriving (Functor, Monad, Applicative)

instance Num a => Num (STNum s a) where
  (+) = liftA2 (+)
  (-) = liftA2 (-)
  (*) = liftA2 (*)
  negate = fmap negate
  signum = fmap signum
  abs = fmap abs
  fromInteger = pure . fromInteger
