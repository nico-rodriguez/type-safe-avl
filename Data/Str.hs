{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Str where

import Compare
import Data.Chr
import Data.Type.Bool
import Data.Type.Equality

data Str :: [Chr] -> * where
  EmptyStr :: Str '[]
  SStr     :: SChar c -> Str cs -> Str (c:cs)

type family CompareStr (a :: [Chr]) (b :: [Chr]) :: Ordering where
  CompareStr '[]    '[]     = 'EQ
  CompareStr '[]    (y:ys)  = 'LT
  CompareStr (x:xs) '[]     = 'GT
  CompareStr (x:xs) (y:ys)  =
    (If (CompareChr x y == 'EQ)
      (CompareStr xs ys)
      (If (CompareChr x y == 'LT)
        'LT
        'GT
      )
    )
type instance Compare (a :: [Chr]) (b :: [Chr]) = CompareStr a b

-- data OWOTO :: [Chr] -> [Chr] -> * where
--   LE :: (CompareStr x y ~ 'LT) => OWOTO x y
--   EE :: (CompareStr x x ~ 'EQ) => OWOTO x x
--   GE :: (CompareStr x y ~ 'GT) => OWOTO x y

owotoStr :: Str m -> Str n -> OWOTO m n
owotoStr EmptyStr    EmptyStr    = EE
owotoStr EmptyStr    (SStr d ds) = LE
owotoStr (SStr c cs) EmptyStr    = GE
owotoStr (SStr c cs) (SStr d ds) = case owotoChr c d of
  LE -> LE
  GE -> GE
  EE -> EE
