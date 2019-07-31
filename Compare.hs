{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PolyKinds #-}

module Compare where

type family Compare (a :: k) (b :: k) :: Ordering

data OWOTO :: a -> b -> * where
  LE :: (Compare x y ~ 'LT) => OWOTO x y
  EE :: (Compare x x ~ 'EQ) => OWOTO x x
  GE :: (Compare x y ~ 'GT) => OWOTO x y

owoto :: s1 a -> s2 b -> OWOTO a b
owoto = undefined
